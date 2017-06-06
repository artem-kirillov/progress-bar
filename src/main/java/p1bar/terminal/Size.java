package p1bar.terminal;

public class Size {
    private final int columns;
    private final int rows;

    public Size(int columns, int rows) {
        if (columns < 0 || rows < 0) {
            throw new IllegalArgumentException("Columns and rows should be non negative. " +
                    "Got columns " + columns + ", rows " + rows);
        }
        this.columns = columns;
        this.rows = rows;
    }

    public int getColumns() {
        return columns;
    }

    public int getRows() {
        return rows;
    }

    @Override
    public String toString() {
        return "Size{" +
                "columns=" + columns +
                ", rows=" + rows +
                '}';
    }
}
