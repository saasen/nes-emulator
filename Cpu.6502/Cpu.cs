using NES.Bus;

// ReSharper disable InconsistentNaming

namespace Cpu6502;

public class Cpu
{
    private readonly IBus _bus;

    private readonly CpuFlags CpuFlags;
    private readonly List<Instruction> _instructions;
    private byte _cycles;
    
    private ushort ProgramCounter;
    private byte AccumulatorRegister = 0x00;
    private byte StackPointer = 0x00;

    private struct Instruction
    {
        public Instruction(string mnemonicCode, Func<byte> opCode, Func<byte> addressingMode, byte cycles)
        {
            MnemonicCode = mnemonicCode;
            AddressingMode = addressingMode;
            OpCode = opCode;
            Cycles = cycles;
        }

        public string MnemonicCode { get; }
        public Func<byte> OpCode { get; }
        public Func<byte> AddressingMode { get; }
        public byte Cycles { get; }
    }

    public Cpu(IBus bus)
    {
        _bus = bus;
        _instructions = new()
        {
            new("BRK", BRK, IMM, 7), new("ORA", ORA, IZX, 6), new("???", XXX, IMP, 2), new("???", XXX, IMP, 8), new("???", NOP, IMP, 3), new("ORA", ORA, ZP0, 3), new("ASL", ASL, ZP0, 5), new("???", XXX, IMP, 5), new("PHP", PHP, IMP, 3), new("ORA", ORA, IMM, 2), new("ASL", ASL, IMP, 2), new("???", XXX, IMP, 2), new("???", NOP, IMP, 4), new("ORA", ORA, ABS, 4), new("ASL", ASL, ABS, 6), new("???", XXX, IMP, 6),
            new("BPL", BPL, REL, 2), new("ORA", ORA, IZY, 5), new("???", XXX, IMP, 2), new("???", XXX, IMP, 8), new("???", NOP, IMP, 4), new("ORA", ORA, ZPX, 4), new("ASL", ASL, ZPX, 6), new("???", XXX, IMP, 6), new("CLC", CLC, IMP, 2), new("ORA", ORA, ABY, 4), new("???", NOP, IMP, 2), new("???", XXX, IMP, 7), new("???", NOP, IMP, 4), new("ORA", ORA, ABX, 4), new("ASL", ASL, ABX, 7), new("???", XXX, IMP, 7),
            new("JSR", JSR, ABS, 6), new("AND", AND, IZX, 6), new("???", XXX, IMP, 2), new("???", XXX, IMP, 8), new("BIT", BIT, ZP0, 3), new("AND", AND, ZP0, 3), new("ROL", ROL, ZP0, 5), new("???", XXX, IMP, 5), new("PLP", PLP, IMP, 4), new("AND", AND, IMM, 2), new("ROL", ROL, IMP, 2), new("???", XXX, IMP, 2), new("BIT", BIT, ABS, 4), new("AND", AND, ABS, 4), new("ROL", ROL, ABS, 6), new("???", XXX, IMP, 6),
            new("BMI", BMI, REL, 2), new("AND", AND, IZY, 5), new("???", XXX, IMP, 2), new("???", XXX, IMP, 8), new("???", NOP, IMP, 4), new("AND", AND, ZPX, 4), new("ROL", ROL, ZPX, 6), new("???", XXX, IMP, 6), new("SEC", SEC, IMP, 2), new("AND", AND, ABY, 4), new("???", NOP, IMP, 2), new("???", XXX, IMP, 7), new("???", NOP, IMP, 4), new("AND", AND, ABX, 4), new("ROL", ROL, ABX, 7), new("???", XXX, IMP, 7),
            new("RTI", RTI, IMP, 6), new("EOR", EOR, IZX, 6), new("???", XXX, IMP, 2), new("???", XXX, IMP, 8), new("???", NOP, IMP, 3), new("EOR", EOR, ZP0, 3), new("LSR", LSR, ZP0, 5), new("???", XXX, IMP, 5), new("PHA", PHA, IMP, 3), new("EOR", EOR, IMM, 2), new("LSR", LSR, IMP, 2), new("???", XXX, IMP, 2), new("JMP", JMP, ABS, 3), new("EOR", EOR, ABS, 4), new("LSR", LSR, ABS, 6), new("???", XXX, IMP, 6),
            new("BVC", BVC, REL, 2), new("EOR", EOR, IZY, 5), new("???", XXX, IMP, 2), new("???", XXX, IMP, 8), new("???", NOP, IMP, 4), new("EOR", EOR, ZPX, 4), new("LSR", LSR, ZPX, 6), new("???", XXX, IMP, 6), new("CLI", CLI, IMP, 2), new("EOR", EOR, ABY, 4), new("???", NOP, IMP, 2), new("???", XXX, IMP, 7), new("???", NOP, IMP, 4), new("EOR", EOR, ABX, 4), new("LSR", LSR, ABX, 7), new("???", XXX, IMP, 7),
            new("RTS", RTS, IMP, 6), new("ADC", ADC, IZX, 6), new("???", XXX, IMP, 2), new("???", XXX, IMP, 8), new("???", NOP, IMP, 3), new("ADC", ADC, ZP0, 3), new("ROR", ROR, ZP0, 5), new("???", XXX, IMP, 5), new("PLA", PLA, IMP, 4), new("ADC", ADC, IMM, 2), new("ROR", ROR, IMP, 2), new("???", XXX, IMP, 2), new("JMP", JMP, IND, 5), new("ADC", ADC, ABS, 4), new("ROR", ROR, ABS, 6), new("???", XXX, IMP, 6),
            new("BVS", BVS, REL, 2), new("ADC", ADC, IZY, 5), new("???", XXX, IMP, 2), new("???", XXX, IMP, 8), new("???", NOP, IMP, 4), new("ADC", ADC, ZPX, 4), new("ROR", ROR, ZPX, 6), new("???", XXX, IMP, 6), new("SEI", SEI, IMP, 2), new("ADC", ADC, ABY, 4), new("???", NOP, IMP, 2), new("???", XXX, IMP, 7), new("???", NOP, IMP, 4), new("ADC", ADC, ABX, 4), new("ROR", ROR, ABX, 7), new("???", XXX, IMP, 7),
            new("???", NOP, IMP, 2), new("STA", STA, IZX, 6), new("???", NOP, IMP, 2), new("???", XXX, IMP, 6), new("STY", STY, ZP0, 3), new("STA", STA, ZP0, 3), new("STX", STX, ZP0, 3), new("???", XXX, IMP, 3), new("DEY", DEY, IMP, 2), new("???", NOP, IMP, 2), new("TXA", TXA, IMP, 2), new("???", XXX, IMP, 2), new("STY", STY, ABS, 4), new("STA", STA, ABS, 4), new("STX", STX, ABS, 4), new("???", XXX, IMP, 4),
            new("BCC", BCC, REL, 2), new("STA", STA, IZY, 6), new("???", XXX, IMP, 2), new("???", XXX, IMP, 6), new("STY", STY, ZPX, 4), new("STA", STA, ZPX, 4), new("STX", STX, ZPY, 4), new("???", XXX, IMP, 4), new("TYA", TYA, IMP, 2), new("STA", STA, ABY, 5), new("TXS", TXS, IMP, 2), new("???", XXX, IMP, 5), new("???", NOP, IMP, 5), new("STA", STA, ABX, 5), new("???", XXX, IMP, 5), new("???", XXX, IMP, 5),
            new("LDY", LDY, IMM, 2), new("LDA", LDA, IZX, 6), new("LDX", LDX, IMM, 2), new("???", XXX, IMP, 6), new("LDY", LDY, ZP0, 3), new("LDA", LDA, ZP0, 3), new("LDX", LDX, ZP0, 3), new("???", XXX, IMP, 3), new("TAY", TAY, IMP, 2), new("LDA", LDA, IMM, 2), new("TAX", TAX, IMP, 2), new("???", XXX, IMP, 2), new("LDY", LDY, ABS, 4), new("LDA", LDA, ABS, 4), new("LDX", LDX, ABS, 4), new("???", XXX, IMP, 4),
            new("BCS", BCS, REL, 2), new("LDA", LDA, IZY, 5), new("???", XXX, IMP, 2), new("???", XXX, IMP, 5), new("LDY", LDY, ZPX, 4), new("LDA", LDA, ZPX, 4), new("LDX", LDX, ZPY, 4), new("???", XXX, IMP, 4), new("CLV", CLV, IMP, 2), new("LDA", LDA, ABY, 4), new("TSX", TSX, IMP, 2), new("???", XXX, IMP, 4), new("LDY", LDY, ABX, 4), new("LDA", LDA, ABX, 4), new("LDX", LDX, ABY, 4), new("???", XXX, IMP, 4),
            new("CPY", CPY, IMM, 2), new("CMP", CMP, IZX, 6), new("???", NOP, IMP, 2), new("???", XXX, IMP, 8), new("CPY", CPY, ZP0, 3), new("CMP", CMP, ZP0, 3), new("DEC", DEC, ZP0, 5), new("???", XXX, IMP, 5), new("INY", INY, IMP, 2), new("CMP", CMP, IMM, 2), new("DEX", DEX, IMP, 2), new("???", XXX, IMP, 2), new("CPY", CPY, ABS, 4), new("CMP", CMP, ABS, 4), new("DEC", DEC, ABS, 6), new("???", XXX, IMP, 6),
            new("BNE", BNE, REL, 2), new("CMP", CMP, IZY, 5), new("???", XXX, IMP, 2), new("???", XXX, IMP, 8), new("???", NOP, IMP, 4), new("CMP", CMP, ZPX, 4), new("DEC", DEC, ZPX, 6), new("???", XXX, IMP, 6), new("CLD", CLD, IMP, 2), new("CMP", CMP, ABY, 4), new("NOP", NOP, IMP, 2), new("???", XXX, IMP, 7), new("???", NOP, IMP, 4), new("CMP", CMP, ABX, 4), new("DEC", DEC, ABX, 7), new("???", XXX, IMP, 7),
            new("CPX", CPX, IMM, 2), new("SBC", SBC, IZX, 6), new("???", NOP, IMP, 2), new("???", XXX, IMP, 8), new("CPX", CPX, ZP0, 3), new("SBC", SBC, ZP0, 3), new("INC", INC, ZP0, 5), new("???", XXX, IMP, 5), new("INX", INX, IMP, 2), new("SBC", SBC, IMM, 2), new("NOP", NOP, IMP, 2), new("???", SBC, IMP, 2), new("CPX", CPX, ABS, 4), new("SBC", SBC, ABS, 4), new("INC", INC, ABS, 6), new("???", XXX, IMP, 6),
            new("BEQ", BEQ, REL, 2), new("SBC", SBC, IZY, 5), new("???", XXX, IMP, 2), new("???", XXX, IMP, 8), new("???", NOP, IMP, 4), new("SBC", SBC, ZPX, 4), new("INC", INC, ZPX, 6), new("???", XXX, IMP, 6), new("SED", SED, IMP, 2), new("SBC", SBC, ABY, 4), new("NOP", NOP, IMP, 2), new("???", XXX, IMP, 7), new("???", NOP, IMP, 4), new("SBC", SBC, ABX, 4), new("INC", INC, ABX, 7), new("???", XXX, IMP, 7)
        };
    }

    public void Write(ushort address, byte data)
    {
        _bus.Write(address, data);
    }

    public byte Read(ushort address)
    {
        return _bus.Read(address);
    }

    public void ClockCycle()
    {
        if (_cycles == 0)
        {
            var opCode = Read(ProgramCounter);
            ProgramCounter++;

            var instruction = _instructions[opCode];
            _cycles = instruction.Cycles;

            var additionalCycle1 = instruction.AddressingMode();
            var additionalCycle2 = instruction.OpCode();

            _cycles += (byte)(additionalCycle1 & additionalCycle2);
        }

        _cycles--;
    }

    public void Reset()
    {
        AccumulatorRegister = 0;
        XRegister = 0;
        YRegister = 0;
        StackPointer = 0xFD;
        StatusRegister = 0x00 | (byte)CpuFlags.U;

        absolute_address = 0xFFFC;
        ushort lo = Read((ushort)(absolute_address + 0));
        ushort hi = Read((ushort)(absolute_address + 1));

        relative_address = 0x0000;
        absolute_address = 0x0000;
        fetched = 0x00;

        _cycles = 8;
    }

    public void InterruptRequestSignal()
    {
        if (GetFlag(CpuFlags.I) == 0)
        {
            Write((ushort)(0x0100 + StackPointer), (byte)((ProgramCounter >> 8) & 0x00FF));
            StackPointer--;
            Write((ushort)(0x0100 + StackPointer), (byte)(ProgramCounter & 0x00FF));
            StackPointer--;
            
            SetFlag(CpuFlags.B, false);
            SetFlag(CpuFlags.U, true);
            SetFlag(CpuFlags.I, true);
            
            Write((ushort)(0x0100 + StackPointer), StatusRegister);
            StackPointer--;

            absolute_address = 0xFFFE;
            ushort lo = Read((ushort)(absolute_address + 0));
            ushort hi = Read((ushort)(absolute_address + 1));
            ProgramCounter = (ushort)((hi << 8) | lo);

            _cycles = 7;
        }
    }

    public void NonMaskableInterruptRequestSignal()
    {
        Write((ushort)(0x0100 + StackPointer), (byte)((ProgramCounter >> 8) & 0x00FF));
        StackPointer--;
        Write((ushort)(0x0100 + StackPointer), (byte)(ProgramCounter & 0x00FF));
        StackPointer--;
            
        SetFlag(CpuFlags.B, false);
        SetFlag(CpuFlags.U, true);
        SetFlag(CpuFlags.I, true);
            
        Write((ushort)(0x0100 + StackPointer), StatusRegister);
        StackPointer--;

        absolute_address = 0xFFFA;
        ushort lo = Read((ushort)(absolute_address + 0));
        ushort hi = Read((ushort)(absolute_address + 1));
        ProgramCounter = (ushort)((hi << 8) | lo);

        _cycles = 8;
    }

    private byte fetched = 0x00;
    private ushort absolute_address = 0x00;
    private ushort relative_address = 0x00;
    private byte opcode = 0x00;
    private byte XRegister = 0x00;
    private byte YRegister = 0x00;
    private byte StatusRegister = 0x00;

    private ushort IncreaseProgramCounter()
    {
        ProgramCounter++;
        return ProgramCounter;
    }

    private byte Fetch()
    {
        if (_instructions[opcode].AddressingMode != IMP)
        {
            fetched = Read(absolute_address);
        }

        return fetched;
    }

    private byte GetFlag(CpuFlags flag)
    {
        return (StatusRegister & (byte)flag) > 0 ? (byte)1 : (byte)0;
    }
    
    private void SetFlag(CpuFlags flag, bool active)
    {
        if (active)
        {
            StatusRegister |= (byte)flag;
        }
        else
        {
            StatusRegister &= (byte)~flag;
        }
    }

    #region Address Modes

    public byte IMP()
    {
        fetched = AccumulatorRegister;
        return 0;
    }

    public byte IMM()
    {
        absolute_address = IncreaseProgramCounter();
        return 0;
    }

    public byte ZP0()
    {
        absolute_address = Read(ProgramCounter);
        IncreaseProgramCounter();
        absolute_address &= 0x00FF;
        return 0;
    }

    public byte ZPX()
    {
        absolute_address = (ushort)(Read(ProgramCounter) + XRegister);
        IncreaseProgramCounter();
        absolute_address &= 0x00FF;        
        return 0;
    }

    public byte ZPY()
    {
        absolute_address = (ushort)(Read(ProgramCounter) + YRegister);
        IncreaseProgramCounter();
        absolute_address &= 0x00FF;        
        return 0;
    }

    public byte REL()
    {
        relative_address = Read(ProgramCounter);
        IncreaseProgramCounter();

        if ((relative_address & 0x80) == 1) // Works?
        {
            relative_address |= 0xFF00;
        }

        return 0;
    }

    public byte ABS()
    {
        var lo = Read(ProgramCounter);
        IncreaseProgramCounter();
        var hi = Read(ProgramCounter);
        IncreaseProgramCounter();

        absolute_address = (ushort)((hi << 8) | lo);
        
        return 0;
    }

    public byte ABX()
    {
        var lo = Read(ProgramCounter);
        IncreaseProgramCounter();
        var hi = Read(ProgramCounter);
        IncreaseProgramCounter();

        absolute_address = (ushort)((hi << 8) | lo);
        absolute_address += XRegister;

        if ((absolute_address & 0xFF00) != hi << 8)
        {
            return 1;
        }

        return 0;
    }

    public byte ABY()
    {
        var lo = Read(ProgramCounter);
        IncreaseProgramCounter();
        var hi = Read(ProgramCounter);
        IncreaseProgramCounter();

        absolute_address = (ushort)((hi << 8) | lo);
        absolute_address += YRegister;

        if ((absolute_address & 0xFF00) != hi << 8)
        {
            return 1;
        }

        return 0;
    }

    public byte IND()
    {
        var pointer_lo = Read(ProgramCounter);
        IncreaseProgramCounter();
        var pointer_hi = Read(ProgramCounter);
        IncreaseProgramCounter();

        var pointer = (ushort)(pointer_hi << 8) | pointer_lo;

        if (pointer_lo == 0x00FF) // Simulate page boundary hardware bug
        {
            absolute_address = (ushort)(Read((ushort)((pointer & 0xFF00) << 8)) | Read((ushort)(pointer + 0)));    
        }
        else // Behave normally
        {
            absolute_address = (ushort)(Read((ushort)((ushort)(pointer + 1) << 8)) | Read((ushort)(pointer + 0)));            
        }
        
        return 0;
    }

    public byte IZX()
    {
        ushort t = Read(ProgramCounter);
        IncreaseProgramCounter();

        ushort lo = Read((ushort)((ushort)(t + XRegister) & 0x00FF));
        ushort hi = Read((ushort)((ushort)(t + XRegister + 1) & 0x00FF));

        absolute_address = (ushort)((hi << 8) | lo);
        
        return 0;
    }

    public byte IZY()
    {
        ushort t = Read(ProgramCounter);
        IncreaseProgramCounter();

        ushort lo = Read((ushort)(t & 0x00FF));
        ushort hi = Read((ushort)((ushort)(t + 1) & 0x00FF));
        
        absolute_address = (ushort)((hi << 8) | lo);
        absolute_address += YRegister;

        if ((absolute_address & 0xFF00) != hi << 8)
        {
            return 1;
        }
        
        return 0;
    }

    public byte XXX()
    {
        return 0x00;
    }

    #endregion

    #region Opcodes

    public byte ADC()
    {
        Fetch();

        ushort temp = (ushort)(AccumulatorRegister + fetched + GetFlag(CpuFlags.C));
        SetFlag(CpuFlags.C, temp > 255);
        SetFlag(CpuFlags.Z, (temp & 0x00FF) == 0);
        SetFlag(CpuFlags.N, (temp & 0x80) == 1);


        var ar = AccumulatorRegister ^ fetched;
        var am = AccumulatorRegister ^ temp;
        var amv = ~(ar & am) & 0x0080;
        
        SetFlag(CpuFlags.V, amv == 1);
        
        AccumulatorRegister = (byte)(temp & 0x00FF);
        
        return 1;
    }

    public byte AND()
    {
        Fetch();
        AccumulatorRegister &= fetched;
        SetFlag(CpuFlags.Z, AccumulatorRegister == 0x00);
        SetFlag(CpuFlags.N, (AccumulatorRegister & 0x80) == 1);
        return 1;
    }

    public byte ASL()
    {
        Fetch();
        ushort temp = (ushort)(fetched << 1);
        
        SetFlag(CpuFlags.C, (temp & 0xFF00) > 0);
        SetFlag(CpuFlags.Z, (temp & 0x00FF) == 0x00);
        SetFlag(CpuFlags.N, (temp & 0x80) == 1);

        if (_instructions[opcode].AddressingMode == IMP)
        {
            AccumulatorRegister = (byte)(temp & 0x00FF);
        }
        else
        {
            Write(absolute_address, (byte)(temp & 0x00FF));
        }

        return 0;
    }

    public byte BCC()
    {
        if (GetFlag(CpuFlags.C) == 0)
        {
            Branch();
        }

        return 0;
    }

    public byte BCS()
    {
        if (GetFlag(CpuFlags.C) == 1)
        {
            Branch();
        }

        return 0;
    }

    public byte BEQ()
    {
        if (GetFlag(CpuFlags.Z) == 1)
        {
            Branch();
        }

        return 0;
    }

    public byte BIT()
    {
        Fetch();
        ushort temp = (ushort)(AccumulatorRegister & fetched);
        SetFlag(CpuFlags.Z, (temp & 0x00FF) == 0x00);
        SetFlag(CpuFlags.N, (fetched & (1 << 7)) == 1); // Compare to 1?
        SetFlag(CpuFlags.V, (fetched & (1 << 6)) == 1); // Compare to 1?

        return 0;
    }

    public byte BMI()
    {
        if (GetFlag(CpuFlags.N) == 1)
        {
            Branch();
        }

        return 0;
    }

    private void Branch()
    {
        _cycles++;
        absolute_address = (ushort)(ProgramCounter + relative_address);

        if ((absolute_address & 0xFF00) != (ProgramCounter & 0xFF00))
        {
            _cycles++;
        }

        ProgramCounter = absolute_address;
    }

    public byte BNE()
    {
        if (GetFlag(CpuFlags.Z) == 1)
        {
            Branch();
        }

        return 0;
    }

    public byte BPL()
    {
        if (GetFlag(CpuFlags.N) == 0)
        {
            Branch();
        }

        return 0;
    }

    public byte BRK()
    {
        IncreaseProgramCounter();
        SetFlag(CpuFlags.I, true);
        Write((ushort)(0x0100 + StackPointer), (byte)((ProgramCounter >> 8) & 0x00FF));
        StackPointer--;
        Write((ushort)(0x0100 + StackPointer), (byte)(ProgramCounter >> 8));
        StackPointer--;
        
        SetFlag(CpuFlags.B, true);
        Write((ushort)(0x0100 + StackPointer), StatusRegister);
        StackPointer--;
        SetFlag(CpuFlags.B, false);

        ProgramCounter = Read((ushort)(0xFFFE | (Read(0xFFFF) >> 8)));

        return 0;
    }

    public byte BVC()
    {
        if (GetFlag(CpuFlags.V) == 0)
        {
            Branch();
        }

        return 0;
    }

    public byte BVS()
    {
        if (GetFlag(CpuFlags.V) == 1)
        {
            Branch();
        }
        
        return 0;
    }

    public byte CLC()
    {
        SetFlag(CpuFlags.C, false);
        return 0;
    }

    public byte CLD()
    {
        SetFlag(CpuFlags.D, false);
        return 0;
    }

    public byte CLI()
    {
        SetFlag(CpuFlags.I, false);
        return 0;
    }

    public byte CLV()
    {
        SetFlag(CpuFlags.V, false);
        return 0;
    }

    public byte CMP()
    {
        Fetch();
        var temp = (ushort)(AccumulatorRegister - fetched);
        SetFlag(CpuFlags.C, AccumulatorRegister >= fetched);
        SetFlag(CpuFlags.Z, (temp & 0x00FF) == 0x0000);
        SetFlag(CpuFlags.N, (temp & 0x0080) == 1);

        return 0;
    }

    public byte CPX()
    {
        Fetch();
        var temp = (ushort)(XRegister - fetched);
        SetFlag(CpuFlags.C, XRegister >= fetched);
        SetFlag(CpuFlags.Z, (temp & 0x00FF) == 0x0000);
        SetFlag(CpuFlags.N, (temp & 0x0080) == 1);

        return 0;
    }

    public byte CPY()
    {
        Fetch();
        var temp = (ushort)(YRegister - fetched);
        SetFlag(CpuFlags.C, YRegister >= fetched);
        SetFlag(CpuFlags.Z, (temp & 0x00FF) == 0x0000);
        SetFlag(CpuFlags.N, (temp & 0x0080) == 1);

        return 0;
    }

    public byte DEC()
    {
        Fetch();
        var temp = fetched - 1;
        Write(absolute_address, (byte)(temp & 0x00FF));
        SetFlag(CpuFlags.Z, (temp & 0x00FF) == 0x0000);
        SetFlag(CpuFlags.N, (temp & 0x0080) == 1);

        return 0;
    }

    public byte DEX()
    {
        XRegister--;
        SetFlag(CpuFlags.Z, XRegister == 0x00);
        SetFlag(CpuFlags.N, (XRegister & 0x80) == 1);

        return 0;
    }

    public byte DEY()
    {
        YRegister--;
        SetFlag(CpuFlags.Z, YRegister == 0x00);
        SetFlag(CpuFlags.N, (YRegister & 0x80) == 1);
        
        return 0;
    }

    public byte EOR()
    {
        Fetch();
        AccumulatorRegister ^= fetched;
        SetFlag(CpuFlags.Z, AccumulatorRegister == 0x00);
        SetFlag(CpuFlags.N, (AccumulatorRegister & 0x80) == 1);
        
        return 1;
    }

    public byte INC()
    {
        Fetch();
        var temp = fetched + 1;
        Write(absolute_address, (byte)(temp & 0x00FF));
        SetFlag(CpuFlags.Z, (temp & 0x00FF) == 0x0000);
        SetFlag(CpuFlags.N, (temp & 0x80) == 1);
        
        return 0;
    }

    public byte INX()
    {
        XRegister++;
        SetFlag(CpuFlags.Z, XRegister == 0x00);
        SetFlag(CpuFlags.N, (XRegister & 0x80) == 1);
        
        return 0;
    }

    public byte INY()
    {
        YRegister++;
        SetFlag(CpuFlags.Z, YRegister == 0x00);
        SetFlag(CpuFlags.N, (YRegister & 0x80) == 1);
        
        return 0;
    }

    public byte JMP()
    {
        ProgramCounter = absolute_address;

        return 0;
    }

    public byte JSR()
    {
        ProgramCounter--;
        
        Write((ushort)(0x0100 + StackPointer), (byte)((ProgramCounter >> 8) & 0x00FF));
        StackPointer--;
        Write((ushort)(0x0100 + StackPointer), (byte)(ProgramCounter & 0x00FF));
        StackPointer--;

        ProgramCounter = absolute_address;
        
        return 0;
    }

    public byte LDA()
    {
        Fetch();
        AccumulatorRegister = fetched;
        SetFlag(CpuFlags.Z, AccumulatorRegister == 0x00);
        SetFlag(CpuFlags.N, (AccumulatorRegister & 0x80) == 1);
        
        return 1;
    }

    public byte LDX()
    {
        Fetch();
        XRegister = fetched;
        SetFlag(CpuFlags.Z, XRegister == 0x00);
        SetFlag(CpuFlags.N, (XRegister & 0x80) == 1);
        
        return 1;
    }

    public byte LDY()
    {
        Fetch();
        YRegister = fetched;
        SetFlag(CpuFlags.Z, YRegister == 0x00);
        SetFlag(CpuFlags.N, (YRegister & 0x80) == 1);
        
        return 1;
    }

    public byte LSR()
    {
        Fetch();
        SetFlag(CpuFlags.C, (fetched & 0x0001) == 1); // ?
        var temp = fetched >> 1;
        SetFlag(CpuFlags.Z, (temp & 0x00FF) == 0x0000);
        SetFlag(CpuFlags.N, (temp & 0x0080) == 1);

        if (_instructions[opcode].AddressingMode == IMP)
        {
            AccumulatorRegister = (byte)(temp & 0x00FF);
        }
        else
        {
            Write(absolute_address, (byte)(temp & 0x00FF));
        }

        return 0;
    }

    public byte NOP()
    {
        switch (opcode)
        {
            case 0x1C:
            case 0x3C:
            case 0x5C:
            case 0x7C:
            case 0xDC:
            case 0xFC:
            {
                return 1;
            }
        }

        return 0;
    }

    public byte ORA()
    {
        Fetch();
        AccumulatorRegister |= fetched;
        SetFlag(CpuFlags.Z, AccumulatorRegister == 0x00);
        SetFlag(CpuFlags.N, (AccumulatorRegister & 0x80) == 1);
        
        return 1;
    }

    public byte PHA()
    {
        Write((ushort)(0x0100 + StackPointer), AccumulatorRegister);
        StackPointer--;
        
        return 0;
    }

    public byte PHP()
    {
        Write((ushort)(0x0100 + StackPointer), (byte)(StatusRegister | (byte)CpuFlags.B | (byte)CpuFlags.U));
        StackPointer--;
    }

    public byte PLA()
    {
        StackPointer++;
        AccumulatorRegister = Read((ushort)(0x0100 + StackPointer));
        SetFlag(CpuFlags.Z, AccumulatorRegister == 0x00);
        SetFlag(CpuFlags.N, (AccumulatorRegister & 0x80) == 1);

        return 0;
    }

    public byte PLP()
    {
        StackPointer++;
        
    }

    public byte ROL()
    {
        return 0x00;
    }

    public byte ROR()
    {
        return 0x00;
    }

    public byte RTI()
    {
        StackPointer--;
        StatusRegister = Read((ushort)(0x0100 + StackPointer));
        StatusRegister &= unchecked((byte)~CpuFlags.B);
        StatusRegister &= unchecked((byte)~CpuFlags.U);

        StackPointer++;
        ProgramCounter = Read((ushort)(0x0100 + StackPointer));
        StackPointer++;
        ProgramCounter |= (ushort)(Read((ushort)(0x0100 + StackPointer)) << 8);

        return 0;
    }

    public byte RTS()
    {
        return 0x00;
    }

    public byte SBC()
    {
        Fetch();

        ushort value = (ushort)(fetched ^ 0x00FF);
        
        ushort temp = (ushort)(AccumulatorRegister + value + GetFlag(CpuFlags.C));
        SetFlag(CpuFlags.C, temp > 0xFF00);
        SetFlag(CpuFlags.Z, (temp & 0x00FF) == 0);
        SetFlag(CpuFlags.N, (temp & 0x80) == 1);

        var ar = temp ^ AccumulatorRegister;
        var am = temp ^ value;
        var amv = ar & am & 0x0080;
        
        SetFlag(CpuFlags.V, amv == 1);
        
        AccumulatorRegister = (byte)(temp & 0x00FF);
        
        return 1;
    }

    public byte SEC()
    {
        return 0x00;
    }

    public byte SED()
    {
        return 0x00;
    }

    public byte SEI()
    {
        return 0x00;
    }

    public byte STA()
    {
        return 0x00;
    }

    public byte STX()
    {
        return 0x00;
    }

    public byte STY()
    {
        return 0x00;
    }

    public byte TAX()
    {
        return 0x00;
    }

    public byte TAY()
    {
        return 0x00;
    }

    public byte TSX()
    {
        return 0x00;
    }

    public byte TXA()
    {
        return 0x00;
    }

    public byte TXS()
    {
        return 0x00;
    }

    public byte TYA()
    {
        return 0x00;
    }

    #endregion
}

enum CpuFlags
{
    C = 1 << 0, // Carry Bit
    Z = 1 << 1, // Zero
    I = 1 << 2, // Disable Interrupts
    D = 1 << 3, // Decimal Mode
    B = 1 << 4, // Break
    U = 1 << 5, // Unused
    V = 1 << 6, // Overflow
    N = 1 << 7, // Negative
}