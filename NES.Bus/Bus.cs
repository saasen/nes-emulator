namespace NES.Bus;

public class Bus : IBus
{
    private readonly byte[] RAM;

    private byte AccumulatorRegister = 0x00;
    private byte XRegister = 0x00;
    private byte YRegister = 0x00;
    private byte StackPointer = 0x00;
    private ushort ProgramCounter = 0x0000;
    private byte StatusRegister = 0x00;
    
    public Bus()
    {
        RAM = new byte[64 * 1024]; // 65.535 or sizeof(ushort)
    }

    public void Write(ushort address, byte data)
    {
        if (address >= 0x0000 && address <= 0xFFFF)
        {
            RAM[address] = data;
        }
    }

    public byte Read(ushort address)
    {
        if (address >= 0x0000 && address <= 0xFFFF)
        {
            return RAM[address];
        }

        return 0x00;
    }
}
