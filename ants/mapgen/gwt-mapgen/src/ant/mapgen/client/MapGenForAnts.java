package ant.mapgen.client;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.CheckBox;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.RadioButton;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.VerticalPanel;

/**
 * Entry point classes define <code>onModuleLoad()</code>.
 */
public class MapGenForAnts implements EntryPoint, ClickHandler {
	
	private MapGenerator mapGenerator = new MapGenerator();
	
	private Button generateMapButton;
	
	private RadioButton set2players;
	private RadioButton set4players;
	private RadioButton set8players;
	
	private CheckBox useBorder;
	private CheckBox useDiagonal;
	private CheckBox forceSquare;
	
	private TextBox maxSize;
	private TextBox minSize;
	private TextBox nbrOfWalkers;
	
	/**
	 * This is the entry point method.
	 */
	public void onModuleLoad() {
		
		final FlowPanel flow = new FlowPanel();
		
		//The generate new map button
		generateMapButton = new Button("Generate new map");
		//generate.setSize("50px", "20px");
		generateMapButton.addClickHandler(this);
		flow.add(generateMapButton);
		
		//Amount of players
		set2players = new RadioButton("players", "2 Players");
		set2players.addClickHandler(this);
		set4players = new RadioButton("players", "4 Players");
		set4players.addClickHandler(this);
		set8players = new RadioButton("players", "8 Players");
		set8players.addClickHandler(this);
		set8players.setValue(true);
		VerticalPanel radioPanel = new VerticalPanel();
		radioPanel.add(set2players);
		radioPanel.add(set4players);
		radioPanel.add(set8players);
		flow.add(radioPanel);
		
		//For border
		useBorder = new CheckBox("Use border");
		useBorder.addClickHandler(this);
		useBorder.setValue(mapGenerator.isBorder());
		flow.add(useBorder);
		
		//For diag mode
		useDiagonal = new CheckBox("Use diagonal mode");
		useDiagonal.addClickHandler(this);
		useDiagonal.setValue(mapGenerator.isDiagMode());
		flow.add(useDiagonal);
		
		//For forcing the map to be square
		forceSquare = new CheckBox("Force square map");
		forceSquare.addClickHandler(this);
		forceSquare.setValue(mapGenerator.isForceSquare());
		flow.add(forceSquare);
		
		//Map size
		Label descr = new Label("Max and Min size of the map:");
		flow.add(descr);
		maxSize = new TextBox();
		maxSize.setText("" + mapGenerator.getMaxMapSize());
		flow.add(maxSize);
		minSize = new TextBox();
		minSize.setText("" + mapGenerator.getMinMapSize());
		flow.add(minSize);
		
		//Map size
		Label descr2 = new Label("Amount of \"walkers\" that should generate the map:");
		flow.add(descr2);
		nbrOfWalkers = new TextBox();
		nbrOfWalkers.setText("" + mapGenerator.getNbrOfWalkers());
		flow.add(nbrOfWalkers);

		RootPanel.get("mapPanel").add(flow);
	}
	
	private void generateNewMap() {
		try {
			int max = Integer.parseInt(maxSize.getText());
			mapGenerator.setMaxMapSize(max);
			int min = Integer.parseInt(minSize.getText());
			mapGenerator.setMinMapSize(min);
			int walkers = Integer.parseInt(nbrOfWalkers.getText());
			mapGenerator.setNbrOfWalkers(walkers);
		}catch (Exception e) {}
		
		try {
			updateMap(mapGenerator.generateMap());
		}
		catch (Exception e) {
			RootPanel.get("mapResults").clear();
			RootPanel.get("mapResults").add(new HTML("Oops something went wrong please try again :("));
			e.printStackTrace();
		}
	}
	
	private void updateMap(int[][] map) {
        char ant = 'a';
        StringBuilder sb = new StringBuilder("<br />\n<br />\n");
        sb.append("rows " + map.length + "<br />\n");
        sb.append("cols " + map[0].length + "<br />\n");
        sb.append("players " + mapGenerator.getNbrOfPlayers() + "<br />\n");

        for (int r = 0; r < map.length; r++) {
            sb.append("m ");
            for (int c = 0; c < map[0].length; c++) {
                switch (map[r][c]) {
                    case MapGenerator.WATER_TILE:
                        sb.append(".");
                        break;
                    case MapGenerator.LAND_TILE:
                        sb.append("%");
                        break;
                    case MapGenerator.ANT_START_POS_TILE:
                        sb.append(ant++);
                        break;
                    case MapGenerator.INVALID_TILE:
                        sb.append(" ");
                        break;
                }
            }
            sb.append("<br />\n");
        }

		HTML text = new HTML(sb.toString());
		RootPanel.get("mapResults").clear();
		RootPanel.get("mapResults").add(text);
	}

	@Override
	public void onClick(ClickEvent event) {
		if (event.getSource().equals(generateMapButton)) {
			generateNewMap();
		}
		else if (event.getSource().equals(set2players)) {
			mapGenerator.setNbrOfPlayers(2);
		}
		else if (event.getSource().equals(set4players)) {
			mapGenerator.setNbrOfPlayers(4);
		}
		else if (event.getSource().equals(set8players)) {
			mapGenerator.setNbrOfPlayers(8);
		}
		else if (event.getSource().equals(useBorder)) {
			mapGenerator.setBorder(!mapGenerator.isBorder());
		}
		else if (event.getSource().equals(useDiagonal)) {
			mapGenerator.setDiagMode(!mapGenerator.isDiagMode());
		}
		else if (event.getSource().equals(forceSquare)) {
			mapGenerator.setForceSquare(!mapGenerator.isForceSquare());
		}
	}
}
