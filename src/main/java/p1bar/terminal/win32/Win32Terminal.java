package p1bar.terminal.win32;

import com.sun.jna.Pointer;
import p1bar.terminal.Pty;
import p1bar.terminal.Size;

public class Win32Terminal implements Pty {
    private static final Pointer consoleOut = Kernel32.INSTANCE.GetStdHandle(Kernel32.STD_OUTPUT_HANDLE);

    @Override
    public Size getSize() {
        final Kernel32.CONSOLE_SCREEN_BUFFER_INFO info = new Kernel32.CONSOLE_SCREEN_BUFFER_INFO();
        Kernel32.INSTANCE.GetConsoleScreenBufferInfo(consoleOut, info);
        return new Size(info.windowWidth(), info.windowHeight());
    }
}
