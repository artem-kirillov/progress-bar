package p1bar.terminal.win32;

import com.sun.jna.LastErrorException;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.win32.StdCallLibrary;
import com.sun.jna.win32.W32APIOptions;

import java.util.Arrays;
import java.util.List;

public interface Kernel32 extends StdCallLibrary {
    Kernel32 INSTANCE = (Kernel32) Native.loadLibrary("kernel32", Kernel32.class, W32APIOptions.UNICODE_OPTIONS);

    int STD_OUTPUT_HANDLE = -11;

    Pointer GetStdHandle(int nStdHandle);

    void GetConsoleScreenBufferInfo(Pointer hConsoleOutput, CONSOLE_SCREEN_BUFFER_INFO lpConsoleScreenBufferInfo) throws LastErrorException;

    class COORD extends Structure implements Structure.ByValue {
        private static String[] fieldOrder = new String[]{"X", "Y"};

        public short X;
        public short Y;

        public COORD() {
        }

        public COORD(short X, short Y) {
            this.X = X;
            this.Y = Y;
        }

        @Override
        protected List<String> getFieldOrder() {
            return Arrays.asList(fieldOrder);
        }
    }

    class SMALL_RECT extends Structure implements Structure.ByValue {
        private static String[] fieldOrder = new String[]{"Left", "Top", "Right", "Bottom"};

        public short Left;
        public short Top;
        public short Right;
        public short Bottom;

        public SMALL_RECT() {
        }

        public SMALL_RECT(short Top, short Left, short Bottom, short Right) {
            this.Top = Top;
            this.Left = Left;
            this.Bottom = Bottom;
            this.Right = Right;
        }

        public SMALL_RECT(SMALL_RECT rect) {
            this.Top = rect.Top;
            this.Left = rect.Left;
            this.Bottom = rect.Bottom;
            this.Right = rect.Right;
        }

        public short width() {
            return (short) (this.Right - this.Left);
        }

        public short height() {
            return (short) (this.Bottom - this.Top);
        }

        @Override
        protected List<String> getFieldOrder() {
            return Arrays.asList(fieldOrder);
        }
    }

    class CONSOLE_SCREEN_BUFFER_INFO extends Structure {
        private static String[] fieldOrder = new String[]{"dwSize", "dwCursorPosition", "wAttributes", "srWindow", "dwMaximumWindowSize"};

        public COORD dwSize;
        public COORD dwCursorPosition;
        public short wAttributes;
        public SMALL_RECT srWindow;
        public COORD dwMaximumWindowSize;

        public CONSOLE_SCREEN_BUFFER_INFO() {
        }

        public CONSOLE_SCREEN_BUFFER_INFO(COORD dwSize, COORD dwCursorPosition, short wAttributes, SMALL_RECT srWindow, COORD dwMaximumWindowSize) {
            this.dwSize = dwSize;
            this.dwCursorPosition = dwCursorPosition;
            this.wAttributes = wAttributes;
            this.srWindow = srWindow;
            this.dwMaximumWindowSize = dwMaximumWindowSize;
        }

        public int windowWidth() {
            return this.srWindow.width() + 1;
        }

        public int windowHeight() {
            return this.srWindow.height() + 1;
        }

        @Override
        protected List<String> getFieldOrder() {
            return Arrays.asList(fieldOrder);
        }
    }
}
