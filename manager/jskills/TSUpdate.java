
import java.io.*;
import java.util.*;

import jskills.*;

class TSUpdate {
    static Set<String> playerIds = new HashSet<String>();
    static ArrayList<ITeam> teams = new ArrayList<ITeam>();
    static ArrayList<Integer> ranks = new ArrayList<Integer>();

    private static int[] listToArray(List<Integer> list) {
        int[] ret = new int[list.size()];
        Iterator<Integer> iter = list.iterator();
        for (int i=0; i < list.size(); i++) {
            ret[i] = iter.next().intValue();
        }
        return ret;
    }

    public static void main(String[] args) {
        BufferedReader stdin = new BufferedReader(new InputStreamReader(
                    System.in));
        while(true) {
            try {
                String line = stdin.readLine();
                String[] words = line.split("\\s+");
                if (words[0].length() == 0 || words[0].startsWith("#")) {
                    continue;
                }
                else if (words[0].equals("P")) {
                    if (playerIds.contains(words[1])) {
                        System.err.println("Duplicate player id found.");
                        System.exit(1);
                    }
                    playerIds.add(words[1]);
                    Player p = new Player(words[1]);
                    int rank = Integer.parseInt(words[2]);
                    double mean = Double.parseDouble(words[3]);
                    double stddev = Double.parseDouble(words[4]);
                    teams.add(new Team(p, new Rating(mean, stddev)));
                    ranks.add(rank);
                }
                else if (words[0].equals("C")) {
                    //GameInfo gameinfo = GameInfo.getDefaultGameInfo();
                    GameInfo gameinfo = new GameInfo(50.0, // mu
                                                     50.0/3.0, // sigma
                                                     50.0/6.0, // beta
                                                     50.0/300.0, // tau
                                                     0.01); // draw probability
                    Map<IPlayer, Rating> results;
                    results = TrueSkillCalculator.calculateNewRatings(gameinfo,
                            teams, listToArray(ranks));

                    for (ITeam t : teams) {
                        for (IPlayer p : t.keySet()) {
                            Rating r = results.get(p);
                            System.out.printf("%s %f %f\n", p,
                                    r.getMean(), r.getStandardDeviation());
                        }
                    }
                    System.exit(0);
                }
                else if (words[0].equals("R")) {
                    playerIds.clear();
                    teams.clear();
                    ranks.clear();
                } else {
                    System.out.println("ERROR: Unrecognized input");
                    System.exit(1);
                }
            } catch (IOException err) {
                System.err.println("IOError: "+ err.getMessage());
                System.exit(1);
            }
        }
    }
}

