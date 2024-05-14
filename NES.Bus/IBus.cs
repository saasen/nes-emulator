namespace NES.Bus;

public interface IBus
{
    void Write(ushort address, byte data);
    byte Read(ushort address);
}