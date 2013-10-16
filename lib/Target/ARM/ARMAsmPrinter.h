//===-- ARMAsmPrinter.h - ARM implementation of AsmPrinter ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef ARMASMPRINTER_H
#define ARMASMPRINTER_H

#include "ARM.h"
#include "ARMTargetMachine.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/Support/Compiler.h"

#include "llvm/MC/MCObjectStreamer.h"
#include "ARMBuildAttrs.h"

namespace llvm {

class MCOperand;

namespace ARM {
  enum DW_ISA {
    DW_ISA_ARM_thumb = 1,
    DW_ISA_ARM_arm = 2
  };
}

// Per section and per symbol attributes are not supported.
// To implement them we would need the ability to delay this emission
// until the assembly file is fully parsed/generated as only then do we
// know the symbol and section numbers.
class AttributeEmitter {
public:
  virtual void MaybeSwitchVendor(StringRef Vendor) = 0;
  virtual void EmitAttribute(unsigned Attribute, unsigned Value) = 0;
  virtual void EmitTextAttribute(unsigned Attribute, StringRef String) = 0;
  virtual void Finish() = 0;
  virtual ~AttributeEmitter() {}
};

class AsmAttributeEmitter : public AttributeEmitter {
  MCStreamer &Streamer;

public:
  AsmAttributeEmitter(MCStreamer &Streamer_) : Streamer(Streamer_) {}
  void MaybeSwitchVendor(StringRef Vendor) { }

  void EmitAttribute(unsigned Attribute, unsigned Value) {
    Streamer.EmitRawText("\t.eabi_attribute " +
                         Twine(Attribute) + ", " + Twine(Value));
  }

  void EmitTextAttribute(unsigned Attribute, StringRef String) {
    switch (Attribute) {
    default: llvm_unreachable("Unsupported Text attribute in ASM Mode");
    case ARMBuildAttrs::CPU_name:
      Streamer.EmitRawText(StringRef("\t.cpu ") + String.lower());
      break;
    /* GAS requires .fpu to be emitted regardless of EABI attribute */
    case ARMBuildAttrs::Advanced_SIMD_arch:
    case ARMBuildAttrs::VFP_arch:
      Streamer.EmitRawText(StringRef("\t.fpu ") + String.lower());
      break;
    }
  }
  void Finish() { }
};

class ObjectAttributeEmitter : public AttributeEmitter {
  // This structure holds all attributes, accounting for
  // their string/numeric value, so we can later emmit them
  // in declaration order, keeping all in the same vector
  struct AttributeItemType {
    enum {
      HiddenAttribute = 0,
      NumericAttribute,
      TextAttribute
    } Type;
    unsigned Tag;
    unsigned IntValue;
    StringRef StringValue;
  };

  MCObjectStreamer &Streamer;
  StringRef CurrentVendor;
  SmallVector<AttributeItemType, 64> Contents;

  // Account for the ULEB/String size of each item,
  // not just the number of items
  size_t ContentsSize;
  // FIXME: this should be in a more generic place, but
  // getULEBSize() is in MCAsmInfo and will be moved to MCDwarf
  size_t getULEBSize(int Value) {
    size_t Size = 0;
    do {
      Value >>= 7;
      Size += sizeof(int8_t); // Is this really necessary?
    } while (Value);
    return Size;
  }

public:
  ObjectAttributeEmitter(MCObjectStreamer &Streamer_) :
    Streamer(Streamer_), CurrentVendor(""), ContentsSize(0) { }

  void MaybeSwitchVendor(StringRef Vendor) {
    assert(!Vendor.empty() && "Vendor cannot be empty.");

    if (CurrentVendor.empty())
      CurrentVendor = Vendor;
    else if (CurrentVendor == Vendor)
      return;
    else
      Finish();

    CurrentVendor = Vendor;

    assert(Contents.size() == 0);
  }

  void EmitAttribute(unsigned Attribute, unsigned Value) {
    AttributeItemType attr = {
      AttributeItemType::NumericAttribute,
      Attribute,
      Value,
      StringRef("")
    };
    ContentsSize += getULEBSize(Attribute);
    ContentsSize += getULEBSize(Value);
    Contents.push_back(attr);
  }

  void EmitTextAttribute(unsigned Attribute, StringRef String) {
    AttributeItemType attr = {
      AttributeItemType::TextAttribute,
      Attribute,
      0,
      String
    };
    ContentsSize += getULEBSize(Attribute);
    // String + \0
    ContentsSize += String.size()+1;

    Contents.push_back(attr);
  }

  void Finish() {
    // Vendor size + Vendor name + '\0'
    const size_t VendorHeaderSize = 4 + CurrentVendor.size() + 1;

    // Tag + Tag Size
    const size_t TagHeaderSize = 1 + 4;

    Streamer.EmitIntValue(VendorHeaderSize + TagHeaderSize + ContentsSize, 4);
    Streamer.EmitBytes(CurrentVendor);
    Streamer.EmitIntValue(0, 1); // '\0'

    Streamer.EmitIntValue(ARMBuildAttrs::File, 1);
    Streamer.EmitIntValue(TagHeaderSize + ContentsSize, 4);

    // Size should have been accounted for already, now
    // emit each field as its type (ULEB or String)
    for (unsigned int i=0; i<Contents.size(); ++i) {
      AttributeItemType item = Contents[i];
      Streamer.EmitULEB128IntValue(item.Tag);
      switch (item.Type) {
      default: llvm_unreachable("Invalid attribute type");
      case AttributeItemType::NumericAttribute:
        Streamer.EmitULEB128IntValue(item.IntValue);
        break;
      case AttributeItemType::TextAttribute:
        Streamer.EmitBytes(item.StringValue.upper());
        Streamer.EmitIntValue(0, 1); // '\0'
        break;
      }
    }

    Contents.clear();
  }
};

class LLVM_LIBRARY_VISIBILITY ARMAsmPrinter : public AsmPrinter {

  /// Subtarget - Keep a pointer to the ARMSubtarget around so that we can
  /// make the right decision when printing asm code for different targets.
  const ARMSubtarget *Subtarget;

  /// AFI - Keep a pointer to ARMFunctionInfo for the current
  /// MachineFunction.
  ARMFunctionInfo *AFI;

  /// MCP - Keep a pointer to constantpool entries of the current
  /// MachineFunction.
  const MachineConstantPool *MCP;

  /// InConstantPool - Maintain state when emitting a sequence of constant
  /// pool entries so we can properly mark them as data regions.
  bool InConstantPool;
public:
  explicit ARMAsmPrinter(TargetMachine &TM, MCStreamer &Streamer)
    : AsmPrinter(TM, Streamer), AFI(NULL), MCP(NULL), InConstantPool(false) {
      Subtarget = &TM.getSubtarget<ARMSubtarget>();
    }

  virtual const char *getPassName() const LLVM_OVERRIDE {
    return "ARM Assembly / Object Emitter";
  }

  void printOperand(const MachineInstr *MI, int OpNum, raw_ostream &O,
                    const char *Modifier = 0);

  virtual bool PrintAsmOperand(const MachineInstr *MI, unsigned OpNum,
                               unsigned AsmVariant, const char *ExtraCode,
                               raw_ostream &O) LLVM_OVERRIDE;
  virtual bool PrintAsmMemoryOperand(const MachineInstr *MI, unsigned OpNum,
                                     unsigned AsmVariant, const char *ExtraCode,
                                     raw_ostream &O) LLVM_OVERRIDE;

  void EmitJumpTable(const MachineInstr *MI);
  void EmitJump2Table(const MachineInstr *MI);
  virtual void EmitInstruction(const MachineInstr *MI) LLVM_OVERRIDE;
  virtual bool runOnMachineFunction(MachineFunction &F) LLVM_OVERRIDE;

  virtual void EmitConstantPool() LLVM_OVERRIDE {
    // we emit constant pools customly!
  }
  virtual void EmitFunctionBodyEnd() LLVM_OVERRIDE;
  virtual void EmitFunctionEntryLabel() LLVM_OVERRIDE;
  virtual void EmitStartOfAsmFile(Module &M) LLVM_OVERRIDE;
  virtual void EmitEndOfAsmFile(Module &M) LLVM_OVERRIDE;
  virtual void EmitXXStructor(const Constant *CV) LLVM_OVERRIDE;

  // lowerOperand - Convert a MachineOperand into the equivalent MCOperand.
  bool lowerOperand(const MachineOperand &MO, MCOperand &MCOp);

private:
  // Helpers for EmitStartOfAsmFile() and EmitEndOfAsmFile()
  void emitAttributes();

  // Helper for ELF .o only
  void emitARMAttributeSection();

  // Generic helper used to emit e.g. ARMv5 mul pseudos
  void EmitPatchedInstruction(const MachineInstr *MI, unsigned TargetOpc);

  void EmitUnwindingInstruction(const MachineInstr *MI);

  // emitPseudoExpansionLowering - tblgen'erated.
  bool emitPseudoExpansionLowering(MCStreamer &OutStreamer,
                                   const MachineInstr *MI);

public:
  /// EmitDwarfRegOp - Emit dwarf register operation.
  virtual void EmitDwarfRegOp(const MachineLocation &MLoc, bool Indirect) const
      LLVM_OVERRIDE;

  virtual unsigned getISAEncoding() LLVM_OVERRIDE {
    // ARM/Darwin adds ISA to the DWARF info for each function.
    if (!Subtarget->isTargetDarwin())
      return 0;
    return Subtarget->isThumb() ?
      ARM::DW_ISA_ARM_thumb : ARM::DW_ISA_ARM_arm;
  }

private:
  MCOperand GetSymbolRef(const MachineOperand &MO, const MCSymbol *Symbol);
  MCSymbol *GetARMJTIPICJumpTableLabel2(unsigned uid, unsigned uid2) const;

  MCSymbol *GetARMSJLJEHLabel() const;

  MCSymbol *GetARMGVSymbol(const GlobalValue *GV);

public:
  /// EmitMachineConstantPoolValue - Print a machine constantpool value to
  /// the .s file.
  virtual void
    EmitMachineConstantPoolValue(MachineConstantPoolValue *MCPV) LLVM_OVERRIDE;
};
} // end namespace llvm

#endif
