
public class Test {

    public static void main(String[] args) {

        MapGenerator map = new MapGenerator();
        int[][] res = map.generateMap();
        map.printMap(res);
    }

    public static void print(int[][] map) {
        unit = 'a';
        StringBuilder sb = new StringBuilder();

        for (int r = 0; r < map.length; r++) {
            sb.append("m ");
            for (int c = 0; c < map[0].length; c++) {
                sb.append(form(map[r][c]));
                //sb.append(getVal4(r, c));
            }
            sb.append("\n");
        }
        System.out.println(sb.toString());
    }

    private static char unit = 'a';
    private static String prefix = " ";
    private static String form(int i) {
        if (i == 1) {
            return prefix + ".";
        }
        else if (i == 0) {
            return prefix + "%";
        }
        else if (i == 2) {
            return prefix + unit++;
        }
        else if (i == -1) {
            return prefix + " ";
        }

        if (i < 10 && i >= 0) {
            return "  " + i;
        }
        else if (i < 100 || i < 0) {
            return " " + i;
        }

        return "" + i;
    }
}