package com.aicontest.visualizer;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.StringTokenizer;

import org.mozilla.javascript.Context;
import org.mozilla.javascript.IdFunctionObject;
import org.mozilla.javascript.IdScriptableObject;
import org.mozilla.javascript.NativeArray;
import org.mozilla.javascript.NativeObject;
import org.mozilla.javascript.Scriptable;
import org.mozilla.javascript.ScriptableObject;
import org.mozilla.javascript.Undefined;

import com.aicontest.visualizer.js.tasks.FunctionExecutionUnit;

@SuppressWarnings("serial")
public class Stream extends IdScriptableObject {

	private static final int Id_visualizerReady = 1, MAX_ID = 1;
	private ScriptableObject replay;
	private NativeObject meta;
	private NativeObject replaydata;
	private NativeObject map;
	private NativeArray ants;
	private NativeArray antLists;
	private NativeArray antList;
	private NativeArray antListOld;
	private BufferedReader br;
	private Ant[][] antMap;
	private boolean interrupted;
	private int players;
	private int row;
	private int turn;
	private int rows;
	private int cols;
	private int id;
	private boolean initialized;

	public Stream(ScriptableObject vis, Visualizer visualizer,
			Scriptable scope, String name) {
		activatePrototypeMap(MAX_ID);
		setPrototype(getObjectPrototype(scope));
		setParentScope(scope);
		ScriptableObject.defineProperty(scope, name, this,
				ScriptableObject.DONTENUM);
		replay = (ScriptableObject) visualizer.invoke(vis, "streamingInit",
				null);
		meta = (NativeObject) replay.get("meta", replay);
		replaydata = (NativeObject) meta.get("replaydata", meta);
		ants = (NativeArray) replaydata.get("ants", replaydata);
		map = (NativeObject) replaydata.get("map", replaydata);
		br = new BufferedReader(new InputStreamReader(System.in));
		turn = -2;
		visualizer.addTask(new FunctionExecutionUnit(vis, "streamingStart",
				null));
	}

	@Override
	public String getClassName() {
		return "Stream";
	}

	public boolean visualizerReady(Object[] args) {
		String line;
		do {
			// tokenize line
			String[] tokens;
			try {
				line = br.readLine();
			} catch (IOException e) {
				return false;
			}
			if (line == null) {
				tokens = new String[0];
			} else {
				line = line.trim().toLowerCase();
				if (line.startsWith("#")) {
					continue;
				}
				StringTokenizer st = new StringTokenizer(line);
				tokens = new String[st.countTokens()];
				for (int i = 0; i < tokens.length; i++) {
					tokens[i] = st.nextToken();
				}
			}
			// filter by keyword
			if (line == null || "turn".equals(tokens[0])
					|| "end".equals(tokens[0])) {
				turn++;
				if (turn == -1) {
					// 'header' turn 0
				} else {
					antListOld = antList;
					antList = new NativeArray(0);
					antLists.put(turn, antLists, antList);
					NativeArray countSet = new NativeArray(players);
					for (int i = 0; i < players; i++) {
						countSet.put(i, countSet, 0);
					}
					NativeArray counts = (NativeArray) replay.get("counts",
							replay);
					counts.put(turn, counts, countSet);
					int duration = (line == null) ? turn - 1 : turn - 2;
					replay.put("duration", replay, duration);
					if (turn == 0) {
						// initialization turn 1
						replay.put("players", replay, players);
						replaydata.put("players", replaydata, players);
						ScriptableObject.callMethod(replay,
								"addMissingMetaData", Context.emptyArgs);
						antMap = new Ant[rows][cols];
						NativeArray scoreSet = new NativeArray(players);
						for (int i = 0; i < players; i++) {
							scoreSet.put(i, scoreSet, new NativeArray(0));
						}
						replaydata.put("scores", replaydata, scoreSet);
						NativeArray fogs = new NativeArray(players);
						for (int i = 0; i < players; i++) {
							fogs.put(i, fogs, new NativeArray(0));
						}
						replay.put("fogs", replay, fogs);
					} else {
						if (initialized) {
							return line != null;
						} else if (duration > 0) {
							initialized = true;
							return line != null;
						}
					}
				}
			} else if ("m".equals(tokens[0])) {
				NativeArray walls = (NativeArray) replay.get("walls", replay);
				NativeArray wallsRow = new NativeArray(tokens[1].length());
				char[] mapLine = tokens[1].toCharArray();
				for (int i = 0; i < mapLine.length; i++) {
					char c = mapLine[i];
					if (c >= 'a' && c <= 'z') {
						int playerId = Character.getNumericValue(c) - 10;
						if (players < playerId + 1) {
							players = playerId + 1;
						}
					}
					wallsRow.put(i, wallsRow, c == '%');
				}
				walls.put(row, walls, wallsRow);
				NativeArray mapRow = (NativeArray) map.get("data", map);
				mapRow.put(row++, mapRow, tokens[1]);
			} else if ("f".equals(tokens[0]) || "a".equals(tokens[0])) {
				// a food item or an ant belonging to a player
				int r = Integer.parseInt(tokens[1]);
				int c = Integer.parseInt(tokens[2]);
				Object owner = "f".equals(tokens[0]) ? Undefined.instance
						: Integer.parseInt(tokens[3]);
				if (owner != Undefined.instance) {
					NativeArray counts = (NativeArray) replay.get("counts",
							replay);
					NativeArray countSet = (NativeArray) counts.get(turn,
							counts);
					countSet.put((Integer) owner, countSet, ((Number) countSet
							.get((Integer) owner, countSet)).intValue() + 1);
				}
				Ant ant = antMap[r][c];
				if (ant == null || ant.toTurn < turn) {
					// spawn new food / ant
					ant = antMap[r][c] = new Ant(replay, id++, r, c, turn,
							owner);
					ants.put(ant.id, ants, ant.js);
					antList.put(antList.size(), antList, ant.js);
					if (antListOld != null && owner == Undefined.instance) {
						antListOld.put(antListOld.size(), antListOld, ant.js);
					}
				} else if (ant.owner == -1) {
					// keep the food another turn / convert to ant
					if (owner != Undefined.instance && ant.owner == -1) {
						ant.owner = (Integer) owner;
						ScriptableObject.callMethod(replay, "convertAnt",
								new Object[] { ant.js, false, turn, owner });
					}
					ant.toTurn++;
					antList.put(antList.size(), antList, ant.js);
				} else if (owner.equals(ant.owner)) {
					ant.toTurn++;
					antList.put(antList.size(), antList, ant.js);
				}
			} else if ("d".equals(tokens[0])) {
				// ants died
				int r = Integer.parseInt(tokens[1]);
				int c = Integer.parseInt(tokens[2]);
				int owner = Integer.parseInt(tokens[3]);
				Ant ant = antMap[r][c];
				if (ant != null && ant.owner == owner && ant.toTurn == turn) {
					ScriptableObject.callMethod(replay, "killAnt",
							new Object[] { ant.js, turn });
				}
				ant = new Ant(replay, id++, r, c, turn, owner);
				ScriptableObject.callMethod(replay, "deadAnt", new Object[] {
						ant.js, turn });
				ants.put(ant.id, ants, ant.js);
				antList.put(antList.size(), antList, ant.js);
				antMap[r][c] = null;
			} else if ("score".equals(tokens[0])) {
				// the scores line for the start of the turn
				NativeArray scoreSet = new NativeArray(players);
				for (int i = 0; i < players; i++) {
					scoreSet.put(i, scoreSet, Integer.parseInt(tokens[i + 1]));
				}
				NativeArray scores = (NativeArray) replay.get("scores", replay);
				scores.put(turn, scores, scoreSet);
				scoreSet = (NativeArray) replaydata.get("scores", replaydata);
				for (int i = 0; i < players; i++) {
					scores = (NativeArray) scoreSet.get(i, scoreSet);
					scores.put(turn, scores, Integer.parseInt(tokens[i + 1]));
				}
			} else if ("players".equals(tokens[0])) {
				// bogus keyword revealing player count to the end of the
				// game we read the player count from the map instead
			} else if ("status".equals(tokens[0])) {
				NativeArray statusArray = new NativeArray(tokens.length - 1);
				for (int i = 1; i < tokens.length; i++) {
					statusArray.put(i - 1, statusArray, tokens[i]);
				}
				meta.put("status", meta, statusArray);
			} else if (tokens.length == 2) {
				// we have some parameter
				int value = Integer.parseInt(tokens[1]);
				if ("rows".equals(tokens[0]) || "cols".equals(tokens[0])) {
					replay.put(tokens[0], replay, value);
					map.put(tokens[0], map, value);
					if ("rows".equals(tokens[0])) {
						map.put("data", map, new NativeArray(value));
						replay.put("walls", replay, new NativeArray(value));
						rows = value;
					} else {
						cols = value;
					}
				} else {
					replaydata.put(tokens[0], replaydata, value);
					if ("turns".equals(tokens[0])) {
						antLists = new NativeArray(value + 1);
						replay.put("turns", replay, antLists);
						NativeArray scores = new NativeArray(value + 1);
						replay.put("scores", replay, scores);
						NativeArray counts = new NativeArray(value + 1);
						replay.put("counts", replay, counts);
					}
				}
			}
		} while (line != null && !interrupted);
		return false;
	}

	@Override
	protected int findPrototypeId(String s) {
		if ("visualizerReady".equals(s)) {
			return Id_visualizerReady;
		}
		return 0;
	}

	@Override
	protected void initPrototypeId(int id) {
		String name;
		int arity;
		switch (id) {
		case Id_visualizerReady:
			arity = 0;
			name = "visualizerReady";
			break;
		default:
			throw new IllegalStateException(String.valueOf(id));
		}
		initPrototypeMethod(null, id, name, arity);
	}

	@Override
	public Object execIdCall(IdFunctionObject f, Context cx, Scriptable scope,
			Scriptable thisObj, Object[] args) {
		int methodId = f.methodId();
		switch (methodId) {
		case Id_visualizerReady:
			return visualizerReady(args);
		default:
			throw new IllegalStateException(String.valueOf(methodId));
		}
	}
}

class Ant {

	int id;
	int owner;
	int toTurn;
	NativeObject js;

	public Ant(ScriptableObject replay, int id, int row, int col, int turn,
			Object owner) {
		this.id = id;
		this.owner = owner == Undefined.instance ? -1 : (Integer) owner;
		this.toTurn = turn + 1;
		if (this.owner == -1) {
			js = (NativeObject) ScriptableObject.callMethod(replay, "spawnAnt",
					new Object[] { id, row, col, turn, owner });
		} else {
			js = (NativeObject) ScriptableObject.callMethod(replay, "spawnAnt",
					new Object[] { id, row, col, 0, owner });
			if (owner != Undefined.instance) {
				ScriptableObject.callMethod(replay, "convertAnt", new Object[] {
						js, true, 0, owner });
			}
		}
	}
}
