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

import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.HashMap;

import org.w3c.dom.Element;

/**
 * Contains graphical and textual data of a word for convenient use in a GUI
 * 
 * @author Sven Zimmer
 * 
 */

public class DepTreeNode {



	// private Elem[] _visibleElems = {Elem.LINE, Elem.TEXT, Elem.DOT,
	// Elem.ARROWLINE1, Elem.ARROWLINE2};

	private String _word;
	private int _index;
	// for every level, this node links to one other or to a root:
	private HashMap<String, Integer> _links = new HashMap<>();
	private HashMap<String, String> _labels = new HashMap<>();
	private HashMap<ElementID, Element> _elements =
			new HashMap<>();
	// each level gets a group of elements that changes color when it is
	// higlighted, for example
	private HashMap<String, ArrayList<Element>> _coloringGroup =
			new HashMap<>();
	private boolean _top;
	private boolean _isInCircle;
	private ArrayList<String> _markedLevels;
	private double _x;
	private double _y;
	private ArrayList<Integer> whoLinks;
	private HashMap <String, Integer>  _nonCurrentYLevel;
	private HashMap <String, Rectangle2D>  _nonCurrentLabelBorders;

	public DepTreeNode(String w) {
		_word = w;
	}

	public void init(ArrayList<String> levels) {
//		setCatText("");
		for (String level : levels) {
			setLink(level, - 1);
			setLabel(level, "");
		}
		setIsTop(false);
		setIsInCircle(false);
		setMarkedLevels(null);
		setX(0);
		setY(0);
		_nonCurrentYLevel = new HashMap<>();
		_nonCurrentLabelBorders = new HashMap<>();
		setWhoLinks(new ArrayList<>());
		_coloringGroup = new HashMap<>();
	}

	public boolean linkIsRoot(String level) {
		return ((_links.get(level) == - 1) || (_links.get(level) == - 2));
	}

	public void setLink(String level, int synLink) {
		this._links.put(level, synLink);
	}

	public int getLink(String level) {
		return _links.get(level);
	}

	public void setElement(Element element, DepTree.Elem type, String level) {
		ElementID id = new ElementID(type, level);
		_elements.put(id, element);
	}

	public Element getElement(DepTree.Elem type, String level) {
		ElementID id = new ElementID(type, level);
		return _elements.get(id);
	}

	private class ElementID {
		DepTree.Elem _type;
		String _level;

		public ElementID(DepTree.Elem type, String level) {
			_type = type;
			_level = level;
		}

		public boolean equals(Object o) {
			if (o.getClass() == ElementID.class) {
				ElementID id = (ElementID) o;
				return equals(id);
			}
			else
				return false;
		}

		public boolean equals(ElementID id) {
			return (_type.equals(id._type) && _level.equals(id._level));
		}

		public int hashCode() {
			return _type.hashCode() + _level.hashCode();
		}
	}

	// each level gets a group of elements that changes color when it is
	// highlighted, for example
	public void addToColoringGroup(Element element, String level) {
		ArrayList<Element> elements = _coloringGroup.get(level);
		if (elements == null) {
			elements = new ArrayList<>();
			_coloringGroup.put(level, elements);
		}
		elements.add(element);
	}

	// TODO decide if to use this
	// public ArrayList<Element> getVisibleElements (){
	// ArrayList<Element> result = new ArrayList<Element>();
	// for (ElementID id : _elementIDs){
	// for (Enum<?> type : _visibleElems){
	// if (id._type.equals(type)){
	// result.add(_elements.get(id));
	// }
	// }
	// }
	// return result;
	// }

	public void setLabel(String level, String label) {
		_labels.put(level, label);
	}

	public String getLabel(String level) {
		return _labels.get(level);
	}

	public ArrayList<Element> getColoringGroup(String level) {
		return _coloringGroup.get(level);
	}

	public void setWord(String _word) {
		this._word = _word;
	}

	public String getWord() {
		return _word;
	}

	public void setIndex(int _index) {
		this._index = _index;
	}

	public int getIndex() {
		return _index;
	}

	public void setIsTop(boolean top) {
		this._top = top;
	}

	public boolean isTop() {
		return _top;
	}

	public void setIsInCircle(boolean isInCircle) {
		this._isInCircle = isInCircle;
	}

	public boolean isInCircle() {
		return _isInCircle;
	}

	public void setMarkedLevels(ArrayList<String> markedLevels) {
		this._markedLevels = markedLevels;
	}

	public ArrayList<String> getMarkedLevels() {
		return _markedLevels;
	}

	public void setX(double x) {
		this._x = x;
	}

	public double getX() {
		return _x;
	}

	public void setY(double y) {
		_y = y;
	}

	public double getY() {
		return _y;
	}

	public void setWhoLinks(ArrayList<Integer> whoLinks) {
		this.whoLinks = whoLinks;
	}

	public ArrayList<Integer> getWhoLinks() {
		return whoLinks;
	}

	public int getNonCurrentYLevel(String level) {
		Integer result = _nonCurrentYLevel.get(level);
		if (result == null){
			result = 0;
		}
		return result;
	}

	public void setNonCurrentYLevel(int nonCurrentYLevel, String level) {
		_nonCurrentYLevel.put(level, nonCurrentYLevel);
	}

	public Rectangle2D getNonCurrentLabelBorder(String level) {
		return _nonCurrentLabelBorders.get(level);
	}

	public void setNonCurrentLabelBorder(Rectangle2D nonCurrentYLabelBorder, String level) {
		_nonCurrentLabelBorders.put(level, nonCurrentYLabelBorder);
	}

}
