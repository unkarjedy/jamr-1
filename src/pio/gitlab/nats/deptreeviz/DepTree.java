/*
 * Copyright  2017  Natural Language Systems Group, Uni Hamburg
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package pio.gitlab.nats.deptreeviz;

import java.awt.*;
import java.awt.geom.*;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.batik.bridge.UpdateManagerAdapter;
import org.apache.batik.bridge.UpdateManagerEvent;
//import org.apache.batik.dom.svg.SVGDOMImplementation;
import org.apache.batik.anim.dom.SVGDOMImplementation;
import org.apache.batik.svggen.SVGGraphics2D;
import org.apache.batik.svggen.SVGGraphics2DIOException;
import org.apache.batik.swing.JSVGCanvas;
import org.apache.batik.swing.gvt.GVTTreeRendererAdapter;
import org.apache.batik.swing.gvt.GVTTreeRendererEvent;
import org.apache.batik.swing.svg.GVTTreeBuilderAdapter;
import org.apache.batik.swing.svg.GVTTreeBuilderEvent;
import org.apache.batik.swing.svg.SVGDocumentLoaderAdapter;
import org.apache.batik.swing.svg.SVGDocumentLoaderEvent;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Element;
import org.w3c.dom.svg.SVGDocument;


/**
 * Contains the graphical information of the tree that results from a parse,
 * plus any listeners that are attached to the elements of the SVGDocument
 * 
 * @author Sven Zimmer and Arne Köhn
 * 
 */
public class DepTree<T extends ParseInterface<E>,E extends WordInterface> implements Iterable<DepTreeNode> {

	private enum BoxType {
		SIMPLE, COMPLEX
	}

	public enum Elem {
		LINE,
		TEXT,
		DOT,
		HIDDENLINE,
		HIDDENBOX,
		HIDDENDOT,
		ARROWLINE1,
		ARROWLINE2
	}

	private ArrayList<DepTreeNode> _wordNodes = new ArrayList<>();
	// only set when using interactions:
	private T _parse;

	private HashMap<CommandID, ArrayList<Element>> _comElements =
				new HashMap<CommandID, ArrayList<Element>>();

	private ArrayList<String> _levels = new ArrayList<>();
	private Map<String, List<String>> _verticesLabels;
	private Map<String, List<Integer>> _verticesStructure;

	private String _currentLevel = "SYN";

	private SVGDocument _doc;

	private int _lineThickness = 2;
	private int _selectThickness = 20;
	private Color _mouseoverColor = new Color(128, 128, 128);
	private Dimension _windowSize = new Dimension(1100, 900);
	private Dimension _canvasSize = _windowSize;
	private Color _highlightColor = new Color(0, 128, 0);
	private Color _selectColor = new Color(128, 128, 255);
	private Color _transparentColor = transWhite;
	private Font _font = new Font("Dialog.plain", 0, 16);

	private static Color darkRed = new Color(128, 0, 0);
	public static Color transWhite = new Color(255, 255, 255, 128);
	public static Color invisible = new Color(0, 0, 0, 0);

	// set true if leaves shall be on the bottom:
	private boolean _droppingLeaves = false;
	// set true if under each word the category shall be drawn (requires _parser
	// to be set):
	private boolean _showingCat = true;
	private boolean _showingReferent = false;
	private boolean _drawingRoot = true;
	// only relevant if a textfield for parsing is used:
	private boolean _parsingIncrementally = true;
	// are transparent Boxes drawn?
	private boolean _drawingBoxes = true;

	private BoxType _boxType = BoxType.SIMPLE;

	private double _currentYLength = 55;
	private double _refYLength = _font.getSize() * 2 + 50;
	// private int _rootY = 40;
	private int _minX = 10;
	// the size of the tree, without the REF links
	private double _minY = 15 + _font.getSize() / 2;
	private double _maxY;
	// the size of everything
	private double _belowTreeY;
	private double _zoomFactor = 1;

	private Map<Integer, ArrayList<String>> _markedNodes;
	private Path2D.Double _transparentBoxes = new Path2D.Double();

	private Stroke _lineStroke;
	private Stroke _selectStroke;
	private Stroke _beforeStroke;

	private SVGGraphics2D _SVGGen;

	// TODO remove for Batik encapsulation
	private JSVGCanvas _nodesCanvas;

	// true while rendering graphics in the _nodesCanvas
	private boolean _busy = false;
	
	// true if the currently checked labels overlap
	private boolean _overlapping = false;

	private ArrayList<DepTreeNode> _forestTODO;
	private ArrayList<DepTreeNode> _nodesTODO;
	private ArrayList<DepTreeNode> _circleTODO;

	private ArrayList<Double> _nonCurrentYs = new ArrayList<>();

	public DepTree() {
	}

	private DepTree(Map<Integer, ArrayList<String>> markedNodes) {
		this();
		_markedNodes = markedNodes;
	}

	public DepTree(
				Map<Integer, ArrayList<String>> markedNodes,
				int windowSizeX,
				int windowSizeY,
				int lineThickness,
				Color highlightColor,
				Color selectColor,
				Font font) {
		this(markedNodes);
		_lineThickness = lineThickness;
		_highlightColor = highlightColor;
		_font = font;
		_windowSize = new Dimension(windowSizeX, windowSizeY);
		_selectColor = selectColor;
	}

	/**
	 * Generates a DepTree from a Parse for extracting the canvas or the DOM tree
	 * from it
	 */
	public DepTree(T parse) {
		this(parse, null);
	}

	/**
	 * Generates a DepTree from a Parse for extracting the canvas or the DOM tree
	 * from it
	 */
	public DepTree(T parse, Map<Integer, ArrayList<String>> markedNodes) {
		this(markedNodes);
		setShowingCat(false);
		draw(parse);
		refreshDisplay();
	}


	@Override
	public Iterator<DepTreeNode> iterator() {
		return _wordNodes.iterator();
	}

	private void add(int i, DepTreeNode node) {
		_wordNodes.add(i, node);
	}

	public DepTreeNode getNode(int i) {
		// System.out.println(i);
		return _wordNodes.get(i);
	}

	private class CommandID {
		DepTreeBaseInteractor.Com _type;
		DepTreeNode _node;
		String _level;
		boolean _isTarget;

		CommandID(
					DepTreeBaseInteractor.Com type,
					DepTreeNode node,
					String level,
					boolean isTarget) {
			_type = type;
			_node = node;
			_level = level;
			_isTarget = isTarget;
		}

		public boolean equals(Object o) {
			if (o.getClass() == CommandID.class) {
				@SuppressWarnings("unchecked")
				CommandID id = (CommandID) o;
				return equals(id);
			}
			else
				return false;
		}

		public boolean equals(CommandID id) {
			if (_node == null) {
				return (_type.equals(id._type) && id._node == null
							&& _level.equals(id._level) && _isTarget == id._isTarget);
			}
			else
				return (_type.equals(id._type) && _node.equals(id._node)
							&& _level.equals(id._level) && _isTarget == id._isTarget);
		}

		public int hashCode() {
			int i = 0;
			if (_isTarget) {
				i = 1;
			}
			int nodecode = 0;
			if (_node != null) {
				nodecode = _node.hashCode();
			}
			return _type.hashCode() + nodecode + _level.hashCode() + i;
		}
	}


	void addElement(Element element,
				DepTreeBaseInteractor.Com type,
				DepTreeNode node,
				String level,
				boolean isTarget) {
		if (element != null) {
			element = (Element) element.getParentNode();
			CommandID id = new CommandID(type, node, level, isTarget);
			ArrayList<Element> elements = _comElements.get(id);
			if (elements == null) {
				elements = new ArrayList<>();
				_comElements.put(id, elements);
			}
			elements.add(element);
		}
	}

	public ArrayList<Element> getElements(DepTreeBaseInteractor.Com type,
				DepTreeNode node,
				String level,
				boolean isTarget) {
		CommandID id = new CommandID(type, node, level, isTarget);
		return _comElements.get(id);
	}

	private Element getCurrentElement() {
		return (Element) _doc.getLastChild()
					.getLastChild()
					.getLastChild()
					.getLastChild();
	}

	private void initCommands() {
		_comElements = new HashMap<CommandID, ArrayList<Element>>();
	}

	private void setFields(T parse) {
		setFields(parse.getLevels(),
					parse.getVerticesLabels(),
					parse.getVerticesStructure());
	}

	private void setFields(List<String> levels,
				   Map<String, List<String>> verticesLabels,
				   Map<String, List<Integer>> verticesStructure) {
		_levels = new ArrayList<>();
		_levels.addAll(levels);
		_levels.add("DEPTREEWORD");
		_levels.add("DEPTREE");
		_verticesLabels = verticesLabels;
		_verticesStructure = verticesStructure;
	}

	private void emptyWordNodes() {
		_wordNodes = new ArrayList<>();
	}

	public void refreshDisplay() {
		if (_nodesCanvas != null) {
			_nodesCanvas.setSVGDocument(_doc);
		}
	}

	private String getCat(DepTreeNode node) {
		return getDecParse().getWords().get(node.getIndex()).getFeature("cat");
	}

	private String getReferent(DepTreeNode node) {
		// TODO see getCat above
		return "Referent_" + node.getIndex();
	}
	public void setLevels(ArrayList<String> levels) {
		this._levels = levels;
	}

	public ArrayList<String> getLevels() {
		return _levels;
	}

	public void setVerticesLabels(Map<String, List<String>> verticesLabels) {
		this._verticesLabels = verticesLabels;
	}

	private Map<String, List<String>> getVerticesLabels() {
		return _verticesLabels;
	}

	public void	setVerticesStructure(Map<String, List<Integer>> verticesStructure) {
		this._verticesStructure = verticesStructure;
	}

	private Map<String, List<Integer>> getVerticesStructure() {
		return _verticesStructure;
	}

	private void setDoc(SVGDocument doc) {
		this._doc = doc;
	}

	public SVGDocument getDoc() {
		return _doc;
	}

	public void setSelectThickness(int selectThickness) {
		this._selectThickness = selectThickness;
	}

	private int getSelectThickness() {
		return _selectThickness;
	}

	public void setMouseoverColor(Color mouseoverColor) {
		this._mouseoverColor = mouseoverColor;
	}

	public Color getMouseoverColor() {
		return _mouseoverColor;
	}

	public void setLineThickness(int lineThickness) {
		this._lineThickness = lineThickness;
	}

	public int getLineThickness() {
		return _lineThickness;
	}

	private void setCanvasSizeX(int canvasSizeX) {
		this._canvasSize =
					new Dimension((int) (canvasSizeX * _zoomFactor),
								_canvasSize.height);
	}

	private int getCanvasSizeX() {
		return _canvasSize.width;
	}

	private void setCanvasSizeY(int canvasSizeY) {
		this._canvasSize =
					new Dimension(_canvasSize.width,
								(int) (canvasSizeY * _zoomFactor));
	}

	public int getCanvasSizeY() {
		return _canvasSize.height;
	}

	public void setWindowSize(Dimension windowSize) {
		this._windowSize = windowSize;
	}

	public Dimension getWindowSize() {
		return _windowSize;
	}

	private void setCanvasSize(Dimension canvasSize) {
		this._canvasSize = canvasSize;
	}

	public Dimension getCanvasSize() {
		return _canvasSize;
	}

	public void setHighlightColor(Color highlightColor) {
		this._highlightColor = highlightColor;
	}

	public Color getHighlightColor() {
		return _highlightColor;
	}

	public void setSelectColor(Color selectColor) {
		this._selectColor = selectColor;
	}

	public Color getSelectColor() {
		return _selectColor;
	}

	public void setFont(Font font) {
		this._font = font;
	}

	public Font getFont() {
		return _font;
	}

	public void setCurrentYLength(double currentYLength) {
		this._currentYLength = currentYLength;
	}

	public double getCurrentYLength() {
		return _currentYLength;
	}

	public void setREFYLength(double refYLength) {
		this._refYLength = refYLength;
	}

	public double getREFYLength() {
		return _refYLength;
	}

	public void setMinX(int minX) {
		this._minX = minX;
	}

	private int getMinX() {
		return _minX;
	}

	public void setMinY(double minY) {
		this._minY = minY;
	}

	public double getMinY() {
		return _minY;
	}

	private void setMaxY(double maxY) {
		this._maxY = maxY;
	}

	private double getMaxY() {
		return _maxY;
	}

	private void setBelowTreeY(double _belowTreeY) {
		this._belowTreeY = _belowTreeY;
	}

	private double getBelowTreeY() {
		return _belowTreeY;
	}

	public void setMarkedNodes(Map<Integer, ArrayList<String>> markedNodes) {
		this._markedNodes = markedNodes;
	}

	public void resetMarkedNodes() {
		this._markedNodes = null;
	}

	public Map<Integer, ArrayList<String>> getMarkedNodes() {
		return _markedNodes;
	}

	private void setLineStroke(Stroke lineStroke) {
		this._lineStroke = lineStroke;
	}

	private Stroke getLineStroke() {
		return _lineStroke;
	}

	private void setSelectStroke(Stroke selectStroke) {
		this._selectStroke = selectStroke;
	}

	private Stroke getSelectStroke() {
		return _selectStroke;
	}

	private void setBeforeStroke(Stroke beforeStroke) {
		this._beforeStroke = beforeStroke;
	}

	private Stroke getBeforeStroke() {
		return _beforeStroke;
	}

	private void set_nodesCanvas(JSVGCanvas nodesCanvas) {
		this._nodesCanvas = nodesCanvas;
		this._nodesCanvas.addGVTTreeRendererListener(new GVTTreeRendererAdapter() {
			public void gvtRenderingPrepare(GVTTreeRendererEvent e) {
				_busy = true;
				// System.out.println("rendering starting");
			}

			public void gvtRenderingCompleted(GVTTreeRendererEvent e) {
				// System.out.println("rendering finished");
				_busy = false;
			}
		});
		this._nodesCanvas.addUpdateManagerListener(new UpdateManagerAdapter() {
			public void updateStarted(UpdateManagerEvent e) {
				// System.out.println("update started");
				_busy = false;
			}
		});
		this._nodesCanvas.addSVGDocumentLoaderListener(new SVGDocumentLoaderAdapter() {
			public void documentLoadingStarted(SVGDocumentLoaderEvent e) {
				_busy = true;
				// System.out.println("Document Loading...");
			}

			public void documentLoadingCompleted(SVGDocumentLoaderEvent e) {
				// System.out.println("Document Loaded.");
				_busy = false;
			}
		});
		this._nodesCanvas.addGVTTreeBuilderListener(new GVTTreeBuilderAdapter() {
			public void gvtBuildStarted(GVTTreeBuilderEvent e) {
				_busy = true;
				// System.out.println("Build Started...");
			}

			public void gvtBuildCompleted(GVTTreeBuilderEvent e) {
				// System.out.println("Build Done.");
				_busy = false;
			}
		});
	}

	public JSVGCanvas getNodesCanvas() {
		return _nodesCanvas;
	}

	private void setForestTODO(ArrayList<DepTreeNode> forestTODO) {
		this._forestTODO = forestTODO;
	}

	private void setNodesTODO(ArrayList<DepTreeNode> nodesTODO) {
		this._nodesTODO = nodesTODO;
	}

	private void setCircleTODO(ArrayList<DepTreeNode> circleTODO) {
		this._circleTODO = circleTODO;
	}

	private ArrayList<DepTreeNode> getCircleTODO() {
		return _circleTODO;
	}

	public void setCurrentLevel(String currentLevel) {
		_currentLevel = currentLevel;
	}

	public String getCurrentLevel() {
		return _currentLevel;
	}

	private void addNonCurrentY(double nonCurrentY) {
		if (_nonCurrentYs.size() == 0) {
			_nonCurrentYs.add(nonCurrentY);
		}
		else if (nonCurrentY > _nonCurrentYs.get(_nonCurrentYs.size() - 1)) {
			_nonCurrentYs.add(nonCurrentY);
		}
	}

	public ArrayList<Double> getNonCurrentYs() {
		return _nonCurrentYs;
	}

	private void resetNonCurrentYs() {
		_nonCurrentYs = new ArrayList<>();
		for (DepTreeNode iNode : this) {
			for (String level : getLevels()){
				iNode.setNonCurrentYLevel(0, level);
			}
		}
	}

	public boolean isDroppingLeaves() {
		return _droppingLeaves;
	}

	public void setDroppingLeaves(boolean droppingLeaves) {
		_droppingLeaves = droppingLeaves;
	}

	public boolean isShowingCat() {
		return _showingCat;
	}

	public void setShowingCat(boolean showingCat) {
		_showingCat = showingCat;
	}

	public T getDecParse() {
		return _parse;
	}

	public void setDecParse(T decParse) {
		_parse = decParse;
	}

	public double getZoomFactor() {
		return _zoomFactor;
	}

	public void setZoomFactor(double zoomFactor) {
		_zoomFactor = zoomFactor;
	}

	private void doZoom(){
		_SVGGen.scale(_zoomFactor, _zoomFactor);
	}

	public boolean isDrawingRoots() {
		return _drawingRoot;
	}

	public void setDrawingRoots(boolean drawRoot) {
		_drawingRoot = drawRoot;
	}

	public Color getTransparentColor() {
		return _transparentColor;
	}

	public void setTransparentColor(Color transparentColor) {
		_transparentColor = transparentColor;
	}

	public boolean isParsingIncrementally() {
		return _parsingIncrementally;
	}

	public void setParsingIncrementally(boolean parsingIncrementally) {
		_parsingIncrementally = parsingIncrementally;
	}

	private Path2D.Double getTransparentBoxes() {
		return _transparentBoxes;
	}

	public void resetTransparentBoxes(Area visibleArea) {
		_transparentBoxes = new Path2D.Double();
	}

	public void addToTransparentBoxes(Shape shape) {
		_transparentBoxes.append(shape, false);
	}

	private boolean isDrawingBoxes() {
		return _drawingBoxes;
	}

	public void setDrawingBoxes(boolean drawingBoxes) {
		_drawingBoxes = drawingBoxes;
	}

	private BoxType getBoxType() {
		return _boxType;
	}

	public void setBoxType(BoxType boxType) {
		_boxType = boxType;
	}

	public boolean isBusy() {
		return _busy;
	}

	public void setBusy(boolean busy) {
		_busy = busy;
	}

	public boolean isShowingReferent() {
		return _showingReferent;
	}

	public void setShowingReferent(boolean showingReferent) {
		_showingReferent = showingReferent;
	}

	private boolean isOverlapping() {
		return _overlapping;
	}

	private void setOverlapping(boolean overlapping) {
		_overlapping = overlapping;
	}


	public void draw(T parse) {
		if (parse == null) {
			initGraph();
		}
		else {
			setFields(parse);
			generateWordNodes(parse);
			initGraph();
			initWordNodes();
			setYPositions(_forestTODO, _nodesTODO);
			buildCircles();
			dropLeaves();
			drawNodes();
		}
	}

	public void redraw() {
		reinitGraph();
		initWordNodes();
		setYPositions(_forestTODO, _nodesTODO);
		buildCircles();
		dropLeaves();
		drawNodes();
	}

	private void initGraph() {
		initCommands();
		setForestTODO(new ArrayList<>());
		setNodesTODO(new ArrayList<>());
		setCircleTODO(new ArrayList<>());
		// Create an SVG document.
		DOMImplementation impl = SVGDOMImplementation.getDOMImplementation();
		String svgNS = SVGDOMImplementation.SVG_NAMESPACE_URI;
		setDoc((SVGDocument) impl.createDocument(svgNS, "svg", null));
		// Create a converter for this document.
		_SVGGen = new SVGGraphics2D(getDoc());
		set_nodesCanvas(new JSVGCanvas());
		getNodesCanvas().setSVGDocument(getDoc());
		_SVGGen.setFont(_font);
		passContent();
	}

	private void reinitGraph() {
		initCommands();
		setForestTODO(new ArrayList<>());
		setNodesTODO(new ArrayList<>());
		setCircleTODO(new ArrayList<>());
		// Create an SVG document.
		DOMImplementation impl = SVGDOMImplementation.getDOMImplementation();
		String svgNS = SVGDOMImplementation.SVG_NAMESPACE_URI;
		setDoc((SVGDocument) impl.createDocument(svgNS, "svg", null));
		getNodesCanvas().setSVGDocument(getDoc());
		// Create a converter for this document.
		_SVGGen = new SVGGraphics2D(getDoc());
		_SVGGen.setFont(_font);
		passContent();
	}

	private void generateWordNodes(T parse) {
		emptyWordNodes();
		List<E> words = parse.getWords();
		for (int i = 0; i < words.size(); i++) {
			WordInterface word = words.get(i);
			DepTreeNode node = new DepTreeNode(word.getWord());
			node.setIndex(i);
			add(i, node);
		}
	}

	private void generateWordNodes( List<String> words) {
		for (int i = 0; i < words.size(); i++) {
			String word = words.get(i);
			DepTreeNode node = new DepTreeNode(word);
			node.setIndex(i);
			add(i, node);
		}
	}

	/**
	 * Assumptions: every Word has a field "from" which is 0 for the first word
	 * and increases by 1 for each following Word, and a field "to" which is
	 * "from" + 1
	 **/
	private void initWordNodes() {
		setMaxY(getCurrentYLength());
		setLineStroke(new BasicStroke(getLineThickness()));
		setSelectStroke(new BasicStroke(getSelectThickness()));

		double iNodeX = _font.getSize() + getMinX(); // x-position of
		// the first word
		if (getMarkedNodes() == null) {
			setMarkedNodes(new HashMap<>());
		}
		for (DepTreeNode node : this) {
			node.init(getLevels());
			node.setMarkedLevels(getMarkedNodes().get(node.getIndex()));
			Rectangle2D wordBorder = _font.getStringBounds(node.getWord(),
					_SVGGen.getFontRenderContext());
			// if categories are drawn below the words then categories that are
			// longer than their words determine the resulting X
			if (isShowingCat()) {
				Rectangle2D catBorder = _font.getStringBounds(
						getCat(node), _SVGGen.getFontRenderContext());
				if (catBorder.getMaxX() > wordBorder.getMaxX()) {
					wordBorder = catBorder;
				}
			}
			if (isShowingReferent()) {
				Rectangle2D referentBorder = _font.getStringBounds(
						getReferent(node), _SVGGen.getFontRenderContext());
				if (referentBorder.getMaxX() > wordBorder.getMaxX()) {
					wordBorder = referentBorder;
				}
			}
			Rectangle2D spaceBorder = _font.getStringBounds("      ",
					_SVGGen.getFontRenderContext());
			double wordLength = spaceBorder.getMaxX() + wordBorder.getMaxX();
			node.setX(iNodeX + wordBorder.getCenterX());
			node.setY(getMinY());
			if (isDrawingRoots()) {
				node.setY(getCurrentYLength() + node.getY());
			}
			iNodeX = iNodeX + wordLength;
			setCanvasSizeX((int) (iNodeX + wordBorder.getMaxX()
					+ getLineThickness() + 1));
			for (String level : getLevels()) {
				if (!level.startsWith("DEPTREE")) {
					node.setLabel(level,
							getVerticesLabels().get(level).get(node.getIndex()));
					node.setLink(
							level,
							getVerticesStructure().get(level)
									.get(node.getIndex()));
				}
			}
			if (node.linkIsRoot(getCurrentLevel())) {
				_forestTODO.add(node);
				node.setIsTop(true);
			}
		}
		for (DepTreeNode nodeA : this) {
			if (!nodeA.isTop()) {
				// add this word A to the list of words that links
				// to the word B linked by this word A
				DepTreeNode nodeB = getNode(nodeA.getLink(getCurrentLevel()));
				ArrayList<Integer> whoLinks = nodeB.getWhoLinks();
				if (whoLinks == null) {
					whoLinks = new ArrayList<>();
					nodeB.setWhoLinks(whoLinks);
				}
				whoLinks.add(nodeA.getIndex());
				_nodesTODO.add(nodeA);
			}
		}
	}

	/**
	 * Sets the y position of the nodes. The first time setYPositions is called, it expects
	 * forestTODO to contain the nodes that are roots and restTODO to contain the
	 * rest. It then traverses each tree starting from each root by removing all
	 * nodes linking to it from restTODO and putting them into forestTODO, while
	 * setting their y one level below it, then repeating that for those nodes
	 * and so on. The nodes left afterwards are in circles (or linking to them),
	 * because none of them links to a root directly or indirectly.
	 * Each time setYPositions is called afterwards, forestTODO is expected to contain one of
	 * those nodes in circles and restTODO the ones that haven't been removed
	 * yet. It then traverses the circle starting from that node as above until
	 * no nodes reaching it are left. Nodes that are left in restTODO are in other circles.
	 * Each circle requires one call of setYPositions.
	 *
	 * @param forestTODO
	 *           contains the nodes that are at the top, because they are known
	 *           to be either roots or a single randomly chosen member of a
	 *           circle. Their y positions are already set
	 * @param restTODO
	 *           contains the other nodes
	 */
	private void setYPositions(ArrayList<DepTreeNode> forestTODO,
							   ArrayList<DepTreeNode> restTODO) {
		while (!forestTODO.isEmpty()) {
			DepTreeNode node = forestTODO.get(0);
			ArrayList<Integer> whoLinks = node.getWhoLinks();
			setMaxY(Math.max(getMaxY(), node.getY()));
			for (Integer ID : whoLinks) {
				DepTreeNode nextNode = getNode(ID);
				if (!nextNode.isTop()) {
					// setMaxY(Math.max(getMaxY(), nextNode.getY()));
					nextNode.setY(node.getY() + getCurrentYLength());
					forestTODO.add(nextNode);
				}
				restTODO.remove(nextNode);
			}
			forestTODO.remove(node);
		}
	}

	/**
	 * Sets the y position of the nodes in the DepTree that are in circles (or
	 * linking to them) and marks them as such nodes. Begins with the node on the
	 * left, uses buidForest to traverse the circle, then repeats with the nodes
	 * that are left until none are left
	 *
	 */
	private void buildCircles() {
		while (!_nodesTODO.isEmpty()) {
			DepTreeNode node = _nodesTODO.get(0);
			while (!node.isInCircle()) {
				node.setIsInCircle(true);
				node = getNode(node.getLink(getCurrentLevel()));
			}
			_nodesTODO.remove(node);
			getCircleTODO().add(node);
			node.setIsTop(true);
			setYPositions(getCircleTODO(), _nodesTODO);
		}
	}

	/**
	 * All leaves are at bottom level, except if it has a brother at a higher
	 * level
	 *
	 */
	private void dropLeaves() {
		if (isDroppingLeaves()) {
			for (DepTreeNode node : this) {
				if ((node.getWhoLinks().isEmpty()) && !node.isTop()) {
					node.setY(getMaxY());
					for (Integer brotherID : getNode(
							node.getLink(getCurrentLevel())).getWhoLinks()) {
						DepTreeNode brother = getNode(brotherID);
						if ((brother.getY() < node.getY())
								&& (((node.getX() < brother.getX()) && (brother
								.getX() < getNode(
								node.getLink(getCurrentLevel())).getX())) || ((node
								.getX() > brother.getX()) && (brother
								.getX() > getNode(
								node.getLink(getCurrentLevel())).getX())))) {
							node.setY(brother.getY());
						}
					}
				}
			}
		}
	}

	/**
	 * Draws the tree.
	 *
	 */
	private void drawNodes() {
		_SVGGen.setPaint(Color.black);
		doZoom();
		// _SVGGen.translate(getMinX(), getMinY());
		setBelowTreeY(getMaxY() + _font.getSize() + 3);
		resetNonCurrentYs();
		setBeforeStroke(_SVGGen.getStroke());

		for (DepTreeNode node : this) {
			drawVerticalLine(node);
		}

		/*
		  draw text labels for SYN first, if complex transparent boxes are used
		 */
		if (getBoxType() == BoxType.COMPLEX) {
			for (DepTreeNode node : this) {
				drawSYNLabel(node);
			}
		}

		/*
		  draw roots and connecting lines
		 */
		for (DepTreeNode node : this) {
			if (node.linkIsRoot(getCurrentLevel())) {
				drawRoot(node);
			}
			else {
				drawCurrentLine(node);
			}
		}

		/*
		 * draw dots, words and categories
		 */
		for (DepTreeNode node : this) {
			drawDot(node, true);
			drawWord(node, true);
			drawCatAndReferent(node, true);
		}

		/*
		 * draw text and line for non current level
		 */
		for (DepTreeNode node : this) {
			drawNoncurrentLine(node, true);
		}

		/*
		 * draw invisible buttons
		 */
		resetNonCurrentYs();
		for (DepTreeNode node : this) {
			drawCurrentLineInvisible(node);
			drawNoncurrentLine(node, false);
			drawWord(node, false);
		}

		/*
		 * draw text labels for SYN
		 */
		if (getBoxType() == BoxType.SIMPLE) {
			for (DepTreeNode node : this) {
				drawSYNLabel(node);
			}
		}

		/*
		 * the invisible dots must be drawn last, so interactions with them always
		 * work
		 */
		for (DepTreeNode node : this) {
			drawDot(node, false);
		}

		// _SVGGen.setPaint(Color.red);
		// _SVGGen.clip(getTransparentBoxes());
		// _SVGGen.fillRect(0, 0, 1000, 1000);

		setCanvasSizeY((int) (getBelowTreeY() + 12 + _font.getSize() * 2));

		setCanvasSize(new Dimension(getCanvasSizeX(), getCanvasSizeY()));
		// getNodesCanvas().setSize(getCanvasSize());
		_SVGGen.setSVGCanvasSize(getCanvasSize());
		// this is important to enable the use of listeners at the elements:
		getNodesCanvas().setDocumentState(JSVGCanvas.ALWAYS_DYNAMIC);
		getNodesCanvas().setEnableRotateInteractor(false);
		getNodesCanvas().setEnableZoomInteractor(false);
		getNodesCanvas().setEnablePanInteractor(false);
		getNodesCanvas().setEnableImageZoomInteractor(false);
		getNodesCanvas().setEnableResetTransformInteractor(false);

		passContent();

		highlightNodes();
	}

	// This is where the generated content is passed to the JSVGCanvas
	// it must be used before adding listeners, to ensure that the node can be
	// found.
	// It generates a new group in the SVG document when used

	private void passContent() {
		Element root = getDoc().getDocumentElement();
		// this takes almost all the calculation time:
		_SVGGen.getRoot(root);
	}

	public void highlightNodes() {
		for (DepTreeNode node : this) {
			highlight(node);
		}
	}

	/**
	 * Colors highlighted links of nodes or resets their color to black or red
	 * for backwards links if they stop being highlighted. This requires the DOM
	 * elements of the corresponding graphic objects to be stored in the right
	 * coloring groups.
	 *
	 * @param node the node worked on
	 */
	public void highlight(DepTreeNode node) {
		for (String level : getLevels()) {
			changeColor(node.getColoringGroup(level), Color.black);
		}
		if (!node.linkIsRoot(getCurrentLevel())) {
			if (node.getY() <= getNode(node.getLink(getCurrentLevel()))
					.getY()) {
				changeColor(node.getColoringGroup(getCurrentLevel()),
						DepTree.darkRed);
			}
		}
		if (!(node.getMarkedLevels() == null)) {
			for (String level : node.getMarkedLevels()) {
				if (level.equals(getCurrentLevel()) || !node.linkIsRoot(level)) {
					changeColor(node.getColoringGroup(level), getHighlightColor());
				}
			}
		}
	}

	/**
	 * Changes the color of the elements of a coloring group
	 *
	 * @param nodeElements
	 *           a list of the DOM elements describing the graphic objects of a
	 *           coloring group
	 */
	public void changeColor(ArrayList<Element> nodeElements, Color color) {
		if (nodeElements != null) {
			for (Element e : nodeElements) {
				// this is needed because directly changing the element instead of
				// it's group creates wrong results
				Element g = (Element) e.getParentNode();
				g.setAttribute("stroke",
						"rgb(" + color.getRed() + "," + color.getGreen() + ","
								+ color.getBlue() + ")");
				g.setAttribute("stroke-opacity", ""
						+ ((float) color.getAlpha() / 255));
				g.setAttribute("fill",
						"rgb(" + color.getRed() + "," + color.getGreen() + ","
								+ color.getBlue() + ")");
				g.setAttribute("fill-opacity", ""
						+ ((float) color.getAlpha() / 255));
			}
		}
	}

	/**
	 * Writes SVG to given writer. This is probably the method you want if you need an SVG.
	 * @param writer receives the SVG. Use StringWriter if you want to get a String.
	 * @throws SVGGraphics2DIOException If for some reason writing failed
	 */
	public void writeTree(Writer writer) throws SVGGraphics2DIOException {
		redraw();
		Element root = getDoc().getDocumentElement();
		root.setAttributeNS(null, "viewBox", "0 0 "
				+ _SVGGen.getSVGCanvasSize().getWidth() + " "
				+ _SVGGen.getSVGCanvasSize().getHeight());
		_SVGGen.stream(getDoc().getDocumentElement(), writer);
	}


	/**
	 * Generates the XML data of a tree from the relevant parts of a parse and
	 * writes it into a file.
	 */
	public static void writeTree(List<String> levels,
								 Map<String, List<String>> verticesLabels,
								 Map<String, List<Integer>> verticesStructure, List<String> words,
								 Writer w) {
		writeTree(levels, verticesLabels, verticesStructure, words, null, w);
	}

	/**
	 * Generates the XML data of a tree from the relevant parts of a parse and
	 * writes it into a file.
	 */
	public static void writeTree(List<String> levels,
						  Map<String, List<String>> verticesLabels,
						  Map<String, List<Integer>> verticesStructure, List<String> words,
						  Map<Integer, ArrayList<String>> markedNodes, Writer w) {
		DepTree<ParseInterface<WordInterface>,WordInterface> dt = new DepTree<>(markedNodes);
		dt.setShowingCat(false);
		dt.setFields(levels, verticesLabels, verticesStructure);
		dt.initGraph();
		dt.generateWordNodes(words);
		dt.initWordNodes();
		dt.setYPositions(dt._forestTODO, dt._nodesTODO);
		dt.buildCircles();
		dt.dropLeaves();
		dt.drawNodes();
		dt.passContent();
		try {
			Element root = dt.getDoc().getDocumentElement();
			root.setAttributeNS(null, "viewBox", "0 0 "
					+ dt._SVGGen.getSVGCanvasSize().getWidth() + " "
					+ dt._SVGGen.getSVGCanvasSize().getHeight());
			dt._SVGGen.stream(dt.getDoc().getDocumentElement(), w);

			// was supposed to write with better formating
			// SVGDocument doc = dt.getDoc();
			// TransformerFactory tf = TransformerFactory.newInstance();
			// Transformer transformer = tf.newTransformer();
			// transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION,
			// "no");
			// transformer.setOutputProperty(OutputKeys.METHOD, "xml");
			// transformer.setOutputProperty(OutputKeys.INDENT, "yes");
			// transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
			// transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount",
			// "4");
			// transformer.transform(new DOMSource(doc),
			// new StreamResult(w));Writes g
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}


	/**
	 * Draw the line from the current node to the one it links to at the current
	 * level in the invisible version. An invisible bigger version of an object
	 * that is at the same coordinates as the visible one is used to be able to
	 * click on a graphical object even when the mouse is somewhat away from it.
	 *
	 * @param node the node worked on
	 */
	private void drawCurrentLineInvisible(DepTreeNode node) {
		_SVGGen.setPaint(DepTree.invisible);
		_SVGGen.setStroke(getSelectStroke());
		// draw button if root
		if (node.linkIsRoot(getCurrentLevel())) {
			Line2D line =
						new Line2D.Double((int) node.getX(),
									0,
									(int) node.getX(),
									(int) node.getY());
			drawShape(node,
						line,
						Elem.HIDDENLINE,
						getCurrentLevel(),
						null,
						false,
						false);
		}
		// draw button if not root
		else {
			double linkX =
						getNode(node.getLink(getCurrentLevel())).getX();
			double linkY =
						getNode(node.getLink(getCurrentLevel())).getY();
			if (node.getY() > linkY) {
				Line2D line =
							new Line2D.Double(node.getX(),
										node.getY(),
										linkX,
										linkY);
				drawShape(
							node,
							line,
							Elem.HIDDENLINE,
							getCurrentLevel(),
							null,
							false,
							false);
			}
			else {
				// draw invisible button for backwards connection in circle
				double sign;
				if (node.getX() < linkX) {
					sign = 1;
				}
				else {
					sign = - 1;
				}
				CubicCurve2D curve =
							new CubicCurve2D.Double(node.getX(),
										node.getY(),
										(node.getX() + linkX) * 2 / 4
													- getCurrentYLength()
													* sign,
										(node.getY() + linkY) / 4
													- getCurrentYLength()
													/ 2,
										(node.getX() + linkX) * 2 / 4
													+ getCurrentYLength()
													* sign,
										(node.getY() + linkY) / 4
													- getCurrentYLength()
													/ 2,
										linkX,
										linkY);
				drawShape(
							node,
							curve,
							Elem.HIDDENLINE,
							getCurrentLevel(),
							null,
							false,
							false);
			}
			_SVGGen.setStroke(getBeforeStroke());
		}
	}

	/**
	 * Draw the text of the word in the sentence if isVisible=true, otherwise
	 * draw an invisible box instead which covers it and the text for its
	 * category, if it exists An invisible bigger version of an object that is
	 * at
	 * the same coordinates as the visible one is used to be able to click on a
	 * graphical object even when the mouse is somewhat away from it.
	 * 
	 * @param node the node worked on
	 * @param isVisible
	 *            is the object itself drawn or its invisible version
	 */
	private void drawWord(DepTreeNode node, boolean isVisible) {
		Rectangle2D wordBorder =
					_font.getStringBounds(node.getWord(),
								_SVGGen.getFontRenderContext());
		if (isVisible) {
			_SVGGen.setPaint(Color.black);
			drawString(node.getWord(), node.getX(), (int) getMaxY()
						+ _font.getSize() + 3, node, "DEPTREEWORD", 0);
		}
		else {
			_SVGGen.setPaint(DepTree.invisible);
			// extend box over category below word if present
			double x = wordBorder.getMaxX() + 2;
			double y = _font.getSize() + 1;
			if (isShowingCat()) {
				// x is the maximum between the length of the word and the
				// length of
				// the category below it
				x = Math.max(x, _font.getStringBounds(getCat(node),_SVGGen.getFontRenderContext()).getMaxX());
				y = _font.getSize() * 2 + 4;
			}
			if (isShowingReferent()) {
				// x is the maximum between the length of the word and the
				// length of
				// the category below it
				x =
							Math.max(x,
										_font
													.getStringBounds(getReferent(node),
																_SVGGen
																			.getFontRenderContext())
													.getMaxX());
				y = _font.getSize() * 2 + 4;
			}
			wordBorder.setRect(node.getX() - 1 - wordBorder.getWidth() / 2,
						getMaxY() + 5,
						x,
						y);
			_SVGGen.fill(wordBorder);
			passContent();
			node.setElement(getCurrentElement(),
						Elem.HIDDENBOX,
						"DEPTREEWORD");
		}
	}

	/**
	 * Draw the category of the word below it if isShowingCat is set, or the 
	 * referent if isShowingReferent is.
	 *
	 * @param node the node worked on
	 */
	private void drawCatAndReferent(DepTreeNode node, boolean isVisible) {
		if (isVisible) {
			int adjustment = 0;
			if (isShowingCat()) {
				drawString(
							getCat(node),
							node.getX(),
							getBelowTreeY() + _font.getSize() + 3,
							node,
							"DEPTREEWORD",
							0);
				adjustment++;
			}
			if (isShowingReferent()) {
				drawString(
							getReferent(node),
							node.getX(),
							getBelowTreeY() + _font.getSize() * (1 + adjustment) + 3 + adjustment,
							node,
							"DEPTREEWORD",
							0);
			}
		}
	}

	/**
	 * Draw the light vertical line from the base of the tree to the dot
	 * belonging to this node.
	 *
	 * @param node the node worked on
	 */
	private void drawVerticalLine(DepTreeNode node) {
		Line2D greyLine =
					new Line2D.Double(node.getX(),
								node.getY(),
								node.getX(),
								getMaxY());
		_SVGGen.setStroke(getLineStroke());
		_SVGGen.setPaint(Color.lightGray);
		drawShape(node, greyLine, null, null, null, true, false);
		_SVGGen.setStroke(getBeforeStroke());
		passContent();
	}

	/**
	 * Draw the dot, label and vertical line belonging to this node if drawing
	 * roots is set in the preferences, assuming that it is a root
	 *
	 * @param node the node worked on
	 */
	private void drawRoot(DepTreeNode node) {
		if (isDrawingRoots()) {
			_SVGGen.setPaint(Color.black);
			_SVGGen.setStroke(getLineStroke());
			Line2D line =
						new Line2D.Double(node.getX(),
									node.getY(),
									node.getX(),
									getMinY());
			double size = getLineThickness() + 5;
			double rad = size / 2;
			Ellipse2D topPoint =
						new Ellipse2D.Double(node.getX() - rad, getMinY()
									- rad, size, size);
			String level = getCurrentLevel();
			drawShape(node, topPoint, null, null, level, true, true);
			drawShape(node, line, Elem.LINE, level, level, true, false);
			String text = "[]";
			if (! "".equals(node.getLabel(level))) {
				text = node.getLabel(level);
			}
			drawString(
						text,
						node.getX() + _font.getSize(),
						(node.getY() - _font.getSize()) / 2
									+ getMinY(),
						node,
						level,
						0);
			_SVGGen.setPaint(Color.black);
		}
	}

	/**
	 * Make objects below a text transparent simply by drawing a transparent box
	 * surrounding its borders before drawing the text
	 *
	 * @param wordBorder
	 *            the border surrounding the text
	 */
	private void drawTransparentSimpleBox(
				Rectangle2D wordBorder) {
		if (isDrawingBoxes() && getBoxType() == BoxType.SIMPLE) {
			_SVGGen.setPaint(getTransparentColor());
			_SVGGen.fill(wordBorder);
			_SVGGen.setPaint(Color.black);
			passContent();
		}
	}

	/**
	 * Make objects below a text transparent by adding its borders to a path
	 * "_transparentBoxes" that is stored in the DepTree afterwards drawing the
	 * text and before drawing all other objects. Drawing them happens twice:
	 * The
	 * second time, a transparent version of the object is drawn, but only
	 * inside
	 * the borders stored in _transparentBoxes. See drawShape.
	 * @param wordBorder
	 *            the border surrounding the text
	 * @param rotation
	 *            the rotation of the text
	 */
	private void drawTransparentComplexBox(
				double x,
				double y,
				Rectangle2D wordBorder,
				double rotation) {
		if (isDrawingBoxes() && getBoxType() == BoxType.COMPLEX) {
			// TODO rotate boxes for SYN labels somehow
			wordBorder.setFrame((int) x,
						(int) y - wordBorder.getHeight() + 4,
						wordBorder.getWidth(),
						wordBorder.getHeight());
			AffineTransform t = _SVGGen.getTransform();
			// t.translate(x, y);
			// t.rotate(rotation);
			// t.translate(-x, -y);
			Path2D.Double box = new Path2D.Double(wordBorder);
			box.transform(t);
			getTransparentBoxes().append(box, false);
		}
	}

	/**
	 * Draw a text with its transparent box, which is either drawn in a simple
	 * manner or a complex one or not at all.
	 */
	private void drawString(
				String text,
				double xCenter,
				double y,
				DepTreeNode node,
				String level,
				double rotation) {
		Rectangle2D wordBorder = getWordBorder(text, xCenter, y);
		double x = xCenter - wordBorder.getWidth() / 2;
		drawTransparentSimpleBox(wordBorder);
		_SVGGen.setStroke(getBeforeStroke());
		_SVGGen.drawString(text, (int) x, (int) y);
		passContent();
		if (level != null) {
			node.setElement(getCurrentElement(), Elem.TEXT, level);
			node.addToColoringGroup(getCurrentElement(), level);
		}
		drawTransparentComplexBox(x, y, wordBorder, rotation);
	}

	private Rectangle2D getWordBorder(String text, double xCenter, double y) {
		Rectangle2D wordBorder =
					_font.getStringBounds(text,
								_SVGGen.getFontRenderContext());
		double x = xCenter - wordBorder.getCenterX();
		wordBorder.setFrame((int) x,
					(int) y - wordBorder.getHeight() + 4,
					wordBorder.getWidth(),
					wordBorder.getHeight());
		return wordBorder;
	}

	/**
	 * Draw a shape. Get the DOM element that was created by the SVGGenerator
	 * and, if desired, store it in the node and put it into an element group so
	 * the correct listeners can be attached to it later for manipulating them
	 * with interactions. Also store it in a coloring group so their colors can
	 * be changed when desired.
	 *
	 * @param elemLevel
	 *            a level like SYN or REF
	 * @param groupLevel
	 *            a level like SYN or REF
	 * @param isVisible
	 *            is the shape drawn visible
	 * @param isFilling
	 *            is the shape drawn by the SVGGenerator with the fill method,
	 *            or
	 *            with the draw method
	 */
	private void drawShape(
				DepTreeNode node,
				Shape s,
				Elem elemType,
				String elemLevel,
				String groupLevel,
				boolean isVisible,
				boolean isFilling) {
		drawOrFillShape(s, isFilling);
		passContent();
		Element element = getCurrentElement();
		if (elemLevel != null && elemType != null) {
			node.setElement(element, elemType, elemLevel);
		}
		if (groupLevel != null) {
			node.addToColoringGroup(element, groupLevel);
		}
		// if transparent boxes are not directly drawn, then simulate them by
		// drawing a second, transparent shape
		// over the current one that is only visible in an area shaped as the
		// box
		if (getBoxType() == BoxType.COMPLEX && isVisible) {
			Shape beforeClip = _SVGGen.getClip();
			Path2D.Double boxes = getTransparentBoxes();
			// double transX = _SVGGen.getTransform().getTranslateX();
			// double transY = _SVGGen.getTransform().getTranslateY();
			// AffineTransform translation = new AffineTransform();
			// translation.translate(transX, transY);
			// boxes.transform(translation);
			_SVGGen.clip(boxes);
			_SVGGen.setColor(getTransparentColor());
			drawOrFillShape(s, isFilling);
			_SVGGen.setColor(Color.black);
			_SVGGen.setClip(beforeClip);
		}
	}

	/**
	 * If a shape is a polygon, then it's filled. If it's a line, then it's
	 * drawn.
	 *
	 * @param isFilling
	 *            is the shape drawn by the SVGGenerator with the fill method,
	 *            or
	 *            with the draw method
	 */
	private void drawOrFillShape(Shape s, boolean isFilling) {
		if (isFilling) {
			_SVGGen.fill(s);
		}
		else {
			_SVGGen.draw(s);
		}
	}

	/**
	 * draw a line from this node to the node it links to at the current level,
	 * assuming it doesn't link the root.
	 *
	 */
	private void drawCurrentLine(DepTreeNode node) {
		_SVGGen.setPaint(Color.black);
		_SVGGen.setStroke(getLineStroke());
		double linkX = getNode(node.getLink(getCurrentLevel())).getX();
		double linkY = getNode(node.getLink(getCurrentLevel())).getY();
		if (node.getY() > linkY) {
			Line2D line =
						new Line2D.Double(node.getX(),
									node.getY(),
									linkX,
									linkY);
			String level = getCurrentLevel();
			drawShape(node, line, Elem.LINE, level, level, true, false);
			_SVGGen.setStroke(getBeforeStroke());
		}
		else {
			// draw backwards connection in circle
			double sign;
			if (node.getX() < linkX) {
				sign = 1;
			}
			else {
				sign = - 1;
			}
			CubicCurve2D curve =
						new CubicCurve2D.Double(linkX,
									linkY,
									(node.getX() + linkX) * 2 / 4
												+ getCurrentYLength() * sign,
									(node.getY() + linkY) / 4
												- getCurrentYLength() / 2,
									(node.getX() + linkX) * 2 / 4
												- getCurrentYLength() * sign,
									(node.getY() + linkY) / 4
												- getCurrentYLength() / 2,
									node.getX(),
									node.getY());
			_SVGGen.setPaint(DepTree.darkRed);
			String level = getCurrentLevel();
			drawShape(node, curve, Elem.LINE, level, level, true, false);
			_SVGGen.setStroke(getBeforeStroke());
		}
		_SVGGen.setPaint(Color.black);
	}

	/**
	 * TODO check for overlapping labels
	 * Draw a curved arrow below the tree from this node to the node it links to
	 * at each non current level if the current level is "SYN", assuming it
	 * doesn't link the root. An invisible bigger version of an object that is
	 * at
	 * the same coordinates as the visible one is used to be able to click on a
	 * graphical object even when the mouse is somewhat away from it.
	 * 
	 * @param node the node worked on
	 * @param isVisible
	 *            is the object itself drawn or its invisible version
	 */
	private void drawNoncurrentLine(DepTreeNode node, boolean isVisible) {
		for (String level : getLevels()) {
			if (getCurrentLevel().equals("SYN")
						&& ! level.equals(getCurrentLevel())
						&& ! level.startsWith("DEPTREE")
						&& ! node.linkIsRoot(level)) {
				// If there is no label for this link, make its text the text of
				// the non current level of the link
				Path2D.Double curve;
				Line2D line1;
				Line2D line2;
				double cx;
				double cy;
				Rectangle2D wordBorder;
				String label = level;
				if (! node.getLabel(level).equals("")) {
					label = label + " : " + node.getLabel(level);
				}
				int arrowHeadWidth = 1 + getLineThickness();
				double refX = getNode(node.getLink(level)).getX();
				double y = getMaxY() + _font.getSize() * 2;
				if (isShowingCat()) {
					y = y + _font.getSize();
				}
				if (isShowingReferent()) {
					y = y + _font.getSize();
				}
				do {
					// if this is the first non current line drawn, set its y at
					// below the tree
					setNonCurrentLevel(node, level);
					if (getNonCurrentYs().size() == 0) {
						addNonCurrentY(y);
					}
					// calculate the lines of the arrow
					double endY =
								getNonCurrentYs()
											.get(node.getNonCurrentYLevel(level) - 1);
					int diff = (int) getREFYLength() / 2;
					// put the left end of the curve slightly (by arrowHeadWidth)
					// to the right and the right end slightly to the left
	//				int leftX = (int) Math.min(refX, node.getX()) + arrowHeadWidth;
	//				int rightX = (int) Math.max(refX, node.getX()) - arrowHeadWidth;
					// same with the arrowhead
					if (refX < node.getX()){
						refX = refX + arrowHeadWidth;
					}
					else {
						refX = refX - arrowHeadWidth;
					}
					curve = new Path2D.Double();
					curve.moveTo(refX, endY);
					curve.lineTo(refX, endY + arrowHeadWidth);
					curve.curveTo(refX,
								endY + diff,
								node.getX(),
								endY + diff,
								node.getX(),
								endY + arrowHeadWidth);
					curve.lineTo(node.getX(), endY);
					line1 =
								new Line2D.Double(refX,
											endY - getLineThickness(),
											refX - arrowHeadWidth,
											endY + 1);
					line2 =
								new Line2D.Double(refX, endY
											- getLineThickness(), refX
											+ arrowHeadWidth, endY + 1);
					Area area = new Area(curve);
					cx = curve.getBounds2D().getCenterX();
					cy = area.getBounds().getMaxY();
	//				Rectangle2D wordBorder =
	//							_font.getStringBounds(label,
	//										_SVGGen.getFontRenderContext());
	//				wordBorder.setFrame((int) (cx - wordBorder.getWidth() * 1 / 2),
	//							(int) cy,
	//							wordBorder.getWidth(),
	//							wordBorder.getHeight());
					wordBorder = getWordBorder(label, cx, cy);
					node.setNonCurrentLabelBorder(wordBorder, level);
					for (DepTreeNode node2 : this) {
						for (String level2 : getLevels()){
							Rectangle2D labelBorder = node.getNonCurrentLabelBorder(level);
							Rectangle2D labelBorder2 = node2.getNonCurrentLabelBorder(level2);
							if (labelBorder != null 
										&& labelBorder2 != null 
										&& (node2 != node || ! level.equals(level2))
										&& ! level2.equals(getCurrentLevel())
										&& labelBorder.intersects(labelBorder2)) {
								setOverlapping(true);
							}
						}
					}
				}
				while (isOverlapping());
				if (isVisible) {
					_SVGGen.setPaint(Color.black);
					_SVGGen.setStroke(getLineStroke());
					drawShape(
								node,
								line1,
								Elem.ARROWLINE1,
								level,
								level,
								true,
								false);
					drawShape(
								node,
								line2,
								Elem.ARROWLINE2,
								level,
								level,
								true,
								false);
					drawShape(
								node,
								curve,
								Elem.LINE,
								level,
								level,
								true,
								false);
					drawString(
								label,
								cx,
								cy + _font.getSize(),
								node,
								level,
								0);
				}
				else {
					_SVGGen.setPaint(DepTree.invisible);
					_SVGGen.setStroke(getSelectStroke());
					drawShape(
								node,
								line1,
								Elem.HIDDENLINE,
								level,
								null,
								false,
								false);
					drawShape(
								node,
								line2,
								Elem.HIDDENLINE,
								level,
								null,
								false,
								false);
					drawShape(
								node,
								curve,
								Elem.HIDDENLINE,
								level,
								null,
								false,
								false);
				}
				_SVGGen.setPaint(Color.BLACK);
				addNonCurrentY(cy + _font.getSize()
							+ getLineThickness() + 2);
				setBelowTreeY(getNonCurrentYs().get(getNonCurrentYs()
							.size() - 1)
							- wordBorder.getHeight());
			}
		}
	}

	/**
	 * If the non current line can be placed next to the other non current
	 * lines instead of below them, do that. Otherwise, place it below the
	 * others.
	 *
	 */
	private void setNonCurrentLevel(DepTreeNode node1, String level1) {
		boolean NonCurLineFits;
		int maxNonCurLevel = 0;
		setOverlapping(false);
		do {
			NonCurLineFits = true;
			node1.setNonCurrentYLevel(node1.getNonCurrentYLevel(level1) + 1, level1);
			for (DepTreeNode node2 : this) {
				for (String level2 : getLevels()){
					if ((node2 != node1 || ! level1.equals(level2)) && ! level2.equals(getCurrentLevel())) {
						maxNonCurLevel =
									Math.max(maxNonCurLevel,
												node2.getNonCurrentYLevel(level2));
						if (node2.getNonCurrentYLevel(level2) == node1.getNonCurrentYLevel(level1)
									&& ! node2.linkIsRoot(level2)) {
							if (IntervalsOverlap(node1.getIndex(),
										node1.getLink(level1),
										node2.getIndex(),
										node2.getLink(level2))) {
								NonCurLineFits = false;
							}
						}
					}
				}
			}
		}
		while (! NonCurLineFits && node1.getNonCurrentYLevel(level1) <= maxNonCurLevel);
	}

	// Does the interval from a to b overlap with the interval from x to y ?
	// Ends are allowed to touch. 
	private boolean IntervalsOverlap(int a, int b, int x, int y) {
		return (! ((Math.max(a, b) <= Math.min(x, y)) || Math.min(a, b) >= Math.max(x,
					y)));
	}

	/**
	 * Draw a label at the line from this node to the node it links to at the
	 * current level, assuming it doesn't link the root, if the current level is
	 * "SYN".
	 * 
	 * @param node the node worked on
	 */
	private void drawSYNLabel(DepTreeNode node) {
		String level = getCurrentLevel();
		if (! node.linkIsRoot(level)) {
			double linkX = getNode(node.getLink(level)).getX();
			double linkY = getNode(node.getLink(level)).getY();
			String text = "[]";
			if (! "".equals(node.getLabel(level))) {
				text = node.getLabel(level);
			}
			if (node.getY() > linkY) {
				float incline =
							((float) (linkX - node.getX()) / (float) (node.getY() - linkY));
				float x = (float) (linkX + ((node.getX() - linkX) / 2));
				float y = (float) (linkY + ((node.getY() - linkY) / 2));
				float rotation;
				if (node.getX() > linkX) {
					rotation = (float) (Math.atan(incline) + Math.PI / 2);
				}
				else {
					rotation = (float) (Math.atan(Math.abs(incline)) - Math.PI / 2);
				}
				_SVGGen.rotate(rotation, x, y);
				_SVGGen.setPaint(getTransparentColor());
				drawString(
							text,
							x,
							y - getLineThickness() - 2,
							node,
							level,
							rotation);
				_SVGGen.rotate(- rotation, x, y);
			}
			else {
				// draw text for backwards connection in circle
				float incline;
				if (! (node.getY() == linkY)) {
					incline = (float) (linkX - node.getX()) / (float) (node.getY() - linkY);
				} else {
					incline = 0;
				}
				float rotation;
				if (node.getX() < linkX) {
					rotation = (float) (Math.atan(incline) + Math.PI / 2);
				} else {
					rotation = (float) (Math.atan(Math.abs(incline)) - Math.PI / 2);
				}
				float x = (float) (node.getX() + linkX) * 2 / 4
										- _font.getSize() * (incline + 1) / 2;
				float y = (float) (node.getY() + linkY) / 4;
				_SVGGen.rotate(rotation, x, y);
				drawString(text, x, y - _font.getSize(), node, level, rotation);
				_SVGGen.rotate(- rotation, x, y);
				_SVGGen.setPaint(Color.black);
			}
			_SVGGen.setPaint(Color.black);
		}
	}

	/**
	 * Draw the dot that each end of a line has. An invisible bigger version of
	 * an object that is at the same coordinates as the visible one is used to
	 * be
	 * able to click on a graphical object even when the mouse is somewhat away
	 * from it.
	 * 
	 * @param node for which to draw the dot
	 * @param isVisible
	 *            is the object itself drawn or its invisible version
	 */
	private void drawDot(DepTreeNode node, boolean isVisible) {
		double size = getLineThickness() + 5;
		Color color = Color.black;
		if (! isVisible) {
			size = getSelectThickness() + 5;
			color = DepTree.invisible;
		}
		double rad = size / 2;
		Ellipse2D point =
					new Ellipse2D.Double(node.getX() - rad,
								node.getY() - rad,
								size,
								size);
		_SVGGen.setPaint(color);
		drawShape(
		          node,
		          point,
		          isVisible? Elem.DOT : Elem.HIDDENDOT,
		          getCurrentLevel(),
		          isVisible? "DEPTREEWORD": null,
		          isVisible,
		          true);
	}
}
