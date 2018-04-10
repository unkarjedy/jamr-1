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

import org.apache.batik.dom.events.DOMMouseEvent;
import org.w3c.dom.Element;
import org.w3c.dom.events.*;

import javax.swing.event.EventListenerList;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class DepTreeBaseInteractor<T extends ParseInterface<E>, E extends WordInterface> {

    public DepTree<T,E> _dt;
    // private DecoratedParse _decParse;
    public NodeSelection _selection = new NodeSelection();
    private boolean _deselect = false;
    private EventListenerList _listeners = new EventListenerList();
    private MouseDragAutomaton _mda = new MouseDragAutomaton();
    private Map<Com, Class<? extends DTMouseEventListener>> _mouseEventListeners;
    private EventListenerList _refreshListeners =
                new EventListenerList();


    protected DepTreeBaseInteractor() {
        _mouseEventListeners = new HashMap<>();
        _mouseEventListeners.put(Com.SELECT, SelectListener.class);
        _mouseEventListeners.put(Com.CHANGETEXT, ChangeTextListener.class);
        _mouseEventListeners.put(Com.CONNECT, ConnectListener.class);
    }

    /**
     * Draws the dependency tree for a parse
     *
     * @param isChanged
     *            is true if the actual content of the parse is changed, instead
     *            of just recoloring nodes so the GUI can change accordingly
     */
    public void draw(boolean isChanged) {
        // if (_dt.getNodesCanvas() != null){
        _dt.draw(_dt.getDecParse());
        addListeners();
        _dt.getNodesCanvas()
                    .addMouseMotionListener(new DTMouseMotionListener());
        _dt.getNodesCanvas().addMouseListener(new DTMouseListener());
        refreshDisplay(isChanged);
        // }
    }

    /**
     * Redraws graph, assuming that the words have not changed, only labels or
     * connections, with a new highlighted selection at the given node and level
     *
     * @param node
     *            the node that has changed
     * @param level
     *            the level that the node has changed at
     * @param isChanged
     *            is true if the actual content of the parse is changed, instead
     *            of just recoloring nodes so the GUI can change accordingly
     */
    public void redraw(DepTreeNode node, String level, boolean isChanged) {
        _dt.redraw();
        addListeners();
        if (node == null) {
            _selection.removeTarget();
        }
        if (_selection.hasTarget()) {
            _dt.changeColor(node.getColoringGroup(level),
                        _dt.getSelectColor());
        }
        refreshDisplay(isChanged);
    }

    /**
     * Refreshes the canvas that displays the dependency tree so it shows
     * changes
     * done to the underlying DOM document
     *
     * @param isChanged
     *            is true if the actual content of the parse is changed, instead
     *            of just recoloring nodes so the GUI can change accordingly
     */
    public void refreshDisplay(boolean isChanged) {
        _dt.refreshDisplay();
        notifyRefresh(isChanged);
    }

    public void addDTRefreshListener(DTRefreshListener listener) {
        _refreshListeners.add(DTRefreshListener.class, listener);
    }

    /**
     * Notifies about refreshing the canvas that displays the dependency tree so
     * it shows changes done to the underlying DOM document
     *
     * @param isChanged
     *            is true if the actual content of the parse is changed, instead
     *            of just recoloring nodes so the GUI can change accordingly
     */
    private synchronized void notifyRefresh(boolean isChanged) {
        for (DTRefreshListener l : _refreshListeners.getListeners(DTRefreshListener.class))
            l.refreshParse(isChanged);
    }

    /**
     * Changes the coordinates of a line in the dependency tree
     *
     * @param x
     * @param y
     * @param e
     *            the element in the DOM tree containing the coordinates for the
     *            line
     */
    private void changeLine(double x, double y, Element e) {
        _dt.getNodesCanvas()
                    .getUpdateManager()
                    .getUpdateRunnableQueue()
                    .invokeLater(new ChangeLineRunnable(x, y, e));
    }

    /**
     * Adjusts coordinates by the current zoom factor
     *
     * @param x
     * @param y
     * @return
     */
    private Dimension adjust(int x, int y) {
        return new Dimension((int) ((x / _dt.getZoomFactor())),
                    (int) ((y / _dt.getZoomFactor())));
    }

    /**
     * Changes the coordinates of a cubic curve in the dependency tree
     *
     * @param x
     * @param y
     * @param e
     *            the element in the DOM tree containing the coordinates for the
     *            line
     */
    private void changeCPath(double x, double y, Element e) {
        _dt.getNodesCanvas()
                    .getUpdateManager()
                    .getUpdateRunnableQueue()
                    .invokeLater(new ChangeCPathRunnable(x, y, e));
    }

    /**
     * Changes the coordinates of a quadratic curve in the dependency tree
     *
     * @param x
     * @param y
     * @param e
     *            the element in the DOM tree containing the coordinates for the
     *            line
     */
    private void changeQPath(double x, double y, Element e) {
        _dt.getNodesCanvas()
                    .getUpdateManager()
                    .getUpdateRunnableQueue()
                    .invokeLater(new ChangeQPathRunnable(x, y, e));
    }

    private void changeColor(Color c, Element e) {
        if (e != null) {
            _dt.getNodesCanvas()
                        .getUpdateManager()
                        .getUpdateRunnableQueue()
                        .invokeLater(new ChangeColorRunnable(c, e));
        }
    }

    public void addPopupListener(PopupListener listener) {
        _listeners.add(PopupListener.class, listener);
    }

    private synchronized void notifySelection(PopupEvent event) {
        for (PopupListener l : _listeners.getListeners(PopupListener.class))
            l.makePopup(event);
    }

    private void addListeners() {
        for (DepTreeNode dtNode : _dt) {
            String currentLevel = _dt.getCurrentLevel();

            Element dot = dtNode.getElement(DepTree.Elem.DOT, currentLevel);
            _dt.addElement(dot, Com.SELECT, dtNode, "DEPTREEWORD", true);
            _dt.addElement(dot, Com.CONNECT, dtNode, currentLevel, true);

            Element hiddendot = dtNode.getElement(DepTree.Elem.HIDDENDOT, currentLevel);
            _dt.addElement(hiddendot, Com.SELECT, dtNode, "DEPTREEWORD", true);
            _dt.addElement(hiddendot, Com.CONNECT, dtNode, currentLevel, true);

            Element text = dtNode.getElement(DepTree.Elem.TEXT, "DEPTREEWORD");
            _dt.addElement(text, Com.SELECT, dtNode, "DEPTREEWORD", true);
            _dt.addElement(text, Com.CONNECT, dtNode, currentLevel, true);
            _dt.addElement(text, Com.CHANGETEXT, dtNode, "DEPTREEWORD", true);

            text = dtNode.getElement(DepTree.Elem.HIDDENBOX, "DEPTREEWORD");
            _dt.addElement(text, Com.SELECT, dtNode, "DEPTREEWORD", true);
            _dt.addElement(text, Com.CONNECT, dtNode, currentLevel, true);
            _dt.addElement(text, Com.CHANGETEXT, dtNode, "DEPTREEWORD", true);

            text = dtNode.getElement(DepTree.Elem.TEXT, currentLevel);
            _dt.addElement(text, Com.CHANGETEXT, dtNode, currentLevel, true);
            _dt.addElement(text, Com.SELECT, dtNode, currentLevel, true);

            for (String level : _dt.getLevels()) {
                if (dtNode.getColoringGroup(level) != null) {
                    for (Element e : dtNode.getColoringGroup(level)) {
                        _dt.addElement(e, Com.SELECT, dtNode, level, true);
                    }
                }
                Element hiddenLine = dtNode.getElement(DepTree.Elem.HIDDENLINE, level);
                _dt.addElement(hiddenLine,
                            Com.SELECT,
                            dtNode,
                            level,
                            false);
            }
        }

        addListeners(true);
        addListeners(false);
    }

    /**
     *
     * @param isTarget
     *            true if the element with the listener also get affected by an
     *            event, for example by changing its color after a selection
     */
    private void addListeners(boolean isTarget) {

        for (Com type : Com.values()) {
            for (String level : _dt.getLevels()) {
                for (DepTreeNode dtNode : _dt) {
                    ArrayList<Element> elements =
                                _dt.getElements(type, dtNode, level, isTarget);
                    if (elements != null) {
                        for (Element e : elements) {
                            addListener(e, type, dtNode, level);
                        }
                    }
                }
                ArrayList<Element> elements =
                            _dt.getElements(type, null, level, isTarget);
                if (elements != null) {
                    for (Element e : elements)
                        addListener(e, type, null, level);
                }
            }
        }
    }

    @SuppressWarnings("incomplete-switch")
    private void addListener(Element e,
                Com com,
                DepTreeNode dtNode,
                String level) {
        EventTarget t = (EventTarget) e;
        DTMouseEventListener listener = null;
        switch (com) {
        case SELECT: {
            listener = getSelectListener();
            break;
        }
        case CHANGETEXT: {
            listener = getChangeTextListener();
            break;
        }
        case CONNECT: {
            listener = getConnectListener();
            break;
        }
        }
        listener.setFields(dtNode, level, com);
        t.addEventListener("click", listener, false);
        if (com == Com.SELECT) {
            t.addEventListener("mouseover", listener, false);
            t.addEventListener("mouseout", listener, false);
        }
    }


    public enum Com {
        SELECT, CONNECT, CHANGETEXT, HIGHLIGHT, DEMO
    }

    /**
     * The states of the mouse drag automaton:
     *
     * NODRAG: no mouse dragging is happening DRAGELEM: the element of a word
     * node is being dragged around DRAGNONE: the mouse is being dragged, but no
     * element has been selected
     *
     */

    private enum MDAState {
        NODRAG, DRAGELEM, DRAGNONE
    }

    public interface DTRefreshListener extends java.util.EventListener {
        void refreshParse(boolean isChanged);
    }

    /**
     * Handles reactions to mouse drags registered by the events and the mouse
     * listener
     *
     */
    private class MouseDragAutomaton {
        MDAState _state = MDAState.NODRAG;
        // node and level of a potential target for a mouse drag
        DepTreeNode _targetNode = null;
        String _targetLevel = null;
        boolean _hasTarget = false;
        // node and level of the element being dragged
        DepTreeNode _draggedNode = null;
        String _draggedLevel = null;
        int _button = 0;

        public MouseDragAutomaton() {
        }

        /**
         * If a select listener at an element notices that the mouse hovers over
         * it, then its node and level becomes a target in the case of a mouse
         * drag
         *
         * @param node
         * @param level
         */
        @SuppressWarnings("incomplete-switch")
        public void moveIn(DepTreeNode node, String level) {
            _hasTarget = true;
            _targetNode = node;
            _targetLevel = level;
            switch (_state) {
            case NODRAG: {
                _dt.changeColor(node.getColoringGroup(level),
                            _dt.getMouseoverColor());
                break;
            }
            }
        }

        /**
         * If a select listener at an element notices that the mouse stops
         * hovering over it, then its node and level stops being a target in the
         * case of a mouse drag
         *
         * @param node
         * @param level
         */
        public void moveOut(DepTreeNode node, String level) {
            if (node.equals(_targetNode) && level.equals(_targetLevel)) {
                _hasTarget = false;
                _targetNode = null;
                _targetLevel = null;
                // color it back to black or, if it is a selected node, to
                // selection
                // color
                _dt.changeColor(node.getColoringGroup(level), Color.black);
                _dt.highlight(node);
                DepTreeNode selectionNode = _selection._target;
                if (selectionNode != null) {
                    _dt.changeColor(selectionNode.getColoringGroup(_selection._level),
                                _dt.getSelectColor());
                }
            }
        }

        /**
         * If the mouse motion listener notices the mouse is being dragged, then
         * mouse dragging is initiated or continued, depending on the state and
         * on
         * the presence of a target
         *
         * @param x
         * @param y
         */
        @SuppressWarnings("incomplete-switch")
        public void dragMouse(Integer x, Integer y) {
            if (_button == 1) {
                Dimension d = adjust(x, y);
                x = d.width;
                y = d.height;
                switch (_state) {
                case NODRAG: {
                    if (_hasTarget && ! "DEPTREEWORD".equals(_targetLevel)) {
                        _state = MDAState.DRAGELEM;
                        _draggedNode = _targetNode;
                        _draggedLevel = _targetLevel;
                    }
                    else {
                        _state = MDAState.DRAGNONE;
                    }
                    break;
                }
                case DRAGELEM: {
                    // if element of tree is dragged
                    if (_draggedLevel.equals(_dt.getCurrentLevel())) {
                        Element t =
                                    _draggedNode.getElement(DepTree.Elem.TEXT,
                                                            _draggedLevel);
                        changeColor(DepTree.invisible, t);
                        // change line if root
                        if (_draggedNode.linkIsRoot(_draggedLevel)) {
                            Element e =
                                        _draggedNode.getElement(DepTree.Elem.LINE,
                                                                _draggedLevel);
                            changeLine(x, y, e);
                        }
                        // change line if not backwards
                        else if (_draggedNode.getY() > _dt.getNode(_draggedNode.getLink(_dt.getCurrentLevel()))
                                    .getY()) {
                            Element e =
                                        _draggedNode.getElement(DepTree.Elem.LINE,
                                                                _draggedLevel);
                            changeLine(x, y, e);
                        }
                        // change backwards line
                        else {
                            Element e =
                                        _draggedNode.getElement(DepTree.Elem.LINE,
                                                                _draggedLevel);
                            changeCPath(x, y, e);
                        }
                    }
                    // if element below tree is dragged
                    else {
                        Element e =
                                    _draggedNode.getElement(DepTree.Elem.LINE,
                                                            _draggedLevel);
                        changeQPath(x, y, e);
                        Element al1 =
                                    _draggedNode.getElement(DepTree.Elem.ARROWLINE1,
                                                            _draggedLevel);
                        changeColor(DepTree.invisible, al1);
                        Element al2 =
                                    _draggedNode.getElement(DepTree.Elem.ARROWLINE2,
                                                            _draggedLevel);
                        changeColor(DepTree.invisible, al2);
                    }
                    break;
                }
                }
            }
        }

        /**
         * if the mouse listener notices the mouse button is being released,
         * then mouse dragging ends either with a connection or an abort,
         * depending on the state and on the presence of a dragging element
         *
         * @param button
         */
        @SuppressWarnings("incomplete-switch")
        public void buttonReleased(int button) {
            _button = button;
            if (_button == 1) {
                switch (_state) {
                case DRAGELEM: {
                    Element e =
                                _draggedNode.getElement(DepTree.Elem.LINE,
                                                        _draggedLevel);
                    if (_hasTarget && "DEPTREEWORD".equals(_targetLevel)) {
                        _state = MDAState.NODRAG;
                        _hasTarget = false;
                        int newLink = - 1;
                        if (_targetNode.getIndex() != _draggedNode.getIndex()) {
                            newLink = _targetNode.getIndex();
                        }
                        _draggedNode.setLink(_targetLevel, newLink);
                        _dt.getDecParse().redirectEdge(_draggedNode.getIndex(),
                                    _draggedLevel,
                                    newLink);
                        redraw(_draggedNode, _draggedLevel, true);
                    }
                    // reset line if mouse points to nothing
                    else {
                        _state = MDAState.NODRAG;
                        _hasTarget = false;
                        // if element of tree has been dragged
                        if (_draggedLevel.equals(_dt.getCurrentLevel())) {
                            Element t =
                                        _draggedNode.getElement(DepTree.Elem.TEXT,
                                                                _draggedLevel);
                            changeColor(_dt.getSelectColor(), t);
                            // reset if root line
                            if (_draggedNode.linkIsRoot(_draggedLevel)) {
                                changeLine(_draggedNode.getX(),
                                            _dt.getMinY(),
                                            e);
                            }
                            // reset if forward line
                            else if (_draggedNode.getY() > _dt.getNode(_draggedNode.getLink(_dt.getCurrentLevel()))
                                        .getY()) {
                                changeLine(_dt.getNode(_draggedNode.getLink(_draggedLevel))
                                            .getX(),
                                            _dt.getNode(_draggedNode.getLink(_draggedLevel))
                                                        .getY(),
                                            e);
                            }
                            // reset backwards line
                            else {
                                changeCPath(_dt.getNode(_draggedNode.getLink(_draggedLevel))
                                            .getX(),
                                            _dt.getNode(_draggedNode.getLink(_draggedLevel))
                                                        .getY(),
                                            e);
                            }
                        }
                        // if element below tree has been dragged
                        else {
                            double linkedNodeXPos = _dt.getNode(_draggedNode.getLink(_draggedLevel)).getX();
                            int arrowHeadWidth = 1 + _dt.getLineThickness();
                            if (linkedNodeXPos < _draggedNode.getX()) {
                                linkedNodeXPos = linkedNodeXPos + arrowHeadWidth;
                            }
                            else {
                                linkedNodeXPos = linkedNodeXPos - arrowHeadWidth;
                            }
                            changeQPath(
                                        linkedNodeXPos,
                                        _dt.getNonCurrentYs().get(_draggedNode.getNonCurrentYLevel(_draggedLevel) - 1),
                                        e);
                            Element al1 =
                                        _draggedNode.getElement(DepTree.Elem.ARROWLINE1,
                                                                _draggedLevel);
                            changeColor(_dt.getSelectColor(), al1);
                            Element al2 =
                                        _draggedNode.getElement(DepTree.Elem.ARROWLINE2,
                                                                _draggedLevel);
                            changeColor(_dt.getSelectColor(), al2);
                        }
                    }
                    break;
                }
                case DRAGNONE: {
                    _state = MDAState.NODRAG;
                    break;
                }
                }
            }
        }

        /*
         * if the mouse listener notices the mouse button is being pressed, then
         * the button is remembered so mouse dragging only is registered when it
         * is done with the left button (1)
         */
        @SuppressWarnings("incomplete-switch")
        public void buttonPressed(int button) {
            switch (_state) {
            case NODRAG: {
                _button = button;
            }
            }
        }
    }

    /**
     * Handles mouse clicks on the whole canvas displaying the dependency tree
     *
     * @author 3zimmer
     *
     */
    private class DTMouseListener implements MouseListener {

        @Override
        public void mouseClicked(MouseEvent mevt) {
        }

        @Override
        public void mouseEntered(MouseEvent mevt) {
        }

        @Override
        public void mouseExited(MouseEvent mevt) {
        }

        @Override
        public void mousePressed(MouseEvent mevt) {
            _mda.buttonPressed(mevt.getButton());
        }

        @Override
        public void mouseReleased(MouseEvent mevt) {
            // inform mouse dragging automaton of button release
            _mda.buttonReleased(mevt.getButton());
            if (_deselect) {
                // TODO deselect when clicking nowhere
                // DepTreeNode node = _selection._target;
                // _dtGen.changeColor(node.getColoringGroup(_selection._level),
                // Color.black);
                // _dtGen.highlight(_dt, node);
                // _selection.setTarget(null, null);
                // _deselect = false;
            }
        }
    }

    /**
     * For thread safe changing of coordinates of a line when dragging it
     *
     * @author 3zimmer
     *
     */
    private class ChangeLineRunnable implements Runnable {
        double _x;
        double _y;
        Element _e;

        /**
         * Changes the coordinates of a line in the dependency tree
         *
         * @param x
         * @param y
         * @param e
         *            the element in the DOM tree containing the coordinates for
         *            the line
         */
        public ChangeLineRunnable(double x, double y, Element e) {
            _x = x;
            _y = y;
            _e = e;
        }

        @Override
        public void run() {
            _e.setAttribute("x2", String.valueOf(_x));
            _e.setAttribute("y2", String.valueOf(_y));
        }
    }

    /**
     * For thread safe changing of coordinates of a cubic curve when dragging it
     *
     * @author 3zimmer
     *
     */
    private class ChangeCPathRunnable implements Runnable {
        double _x;
        double _y;
        Element _e;

        /**
         * Changes the coordinates of a cubic curve in the dependency tree
         *
         * @param x
         * @param y
         * @param e
         *            the element in the DOM tree containing the coordinates for
         *            the line
         */
        public ChangeCPathRunnable(double x, double y, Element e) {
            _x = x;
            _y = y;
            _e = e;
        }

        @Override
        public void run() {
            String[] list = _e.getAttribute("d").split("C");
            _e.setAttribute("d", "M" + _x + " " + _y + "C" + list[1]);
        }
    }

    /**
     * For thread safe changing of coordinates of a quadratic curve when
     * dragging
     * it
     *
     * @author 3zimmer
     *
     */
    private class ChangeQPathRunnable implements Runnable {
        double _x;
        double _y;
        Element _e;

        /**
         * Changes the coordinates of a quadratic curve in the dependency tree
         *
         * @param x
         * @param y
         * @param e
         *            the element in the DOM tree containing the coordinates for
         *            the line
         */
        public ChangeQPathRunnable(double x, double y, Element e) {
            _x = x;
            _y = y;
            _e = e;
        }

        @Override
        public void run() {
            String[] list = _e.getAttribute("d").split("C");
            _e.setAttribute("d", "M" + _x + " " + _y + "C" + list[1]);
        }
    }

    /**
     * For thread safe changing of a color of a shape
     *
     * @author 3zimmer
     *
     */
    private class ChangeColorRunnable implements Runnable {
        Color _c;
        Element _e;

        public ChangeColorRunnable(Color c, Element e) {
            _c = c;
            _e = e;
        }

        @Override
        public void run() {
            String color =
                        "rgb(" + _c.getRed() + "," + _c.getGreen() + ","
                                    + _c.getBlue() + ")";
            Element parent = ((Element) _e.getParentNode());
            parent.setAttribute("fill", color);
            parent.setAttribute("fill-opacity",
                        String.valueOf(_c.getAlpha() / 255));
            parent.setAttribute("stroke", String.valueOf(color));
            parent.setAttribute("stroke-opacity",
                        String.valueOf(_c.getAlpha() / 255));
        }
    }

    /**
     * Used for registering the dragging of the mouse over the canvas containing
     * the dependency tree
     *
     * @author 3zimmer
     *
     */
    private class DTMouseMotionListener implements MouseMotionListener {

        @Override
        public void mouseDragged(MouseEvent mevt) {
            // inform mouse dragging automaton of mouse dragging
            _mda.dragMouse(mevt.getX(), mevt.getY());
        }

        @Override
        public void mouseMoved(MouseEvent mevt) {
        }
    }

    /**
     * Used for selection events on a graphical element of the dependency tree.
     *
     * @author 3zimmer
     *
     */
    protected class SelectListener extends DTMouseEventListener {
        public void act(DOMMouseEvent devt) {
            // inform mouse dragging automaton of mouse movement
            if ("mouseout".equals(devt.getType())) {
                _mda.moveOut(_node, _level);
            }
            else if ("mouseover".equals(devt.getType())) {
                _mda.moveIn(_node, _level);
            }
            // if left button was pressed then select node
            if (devt.getButton() == 0 && "click".equals(devt.getType())) {
                _deselect = false;
                if (_selection.hasTarget()) {
                    DepTreeNode node = _selection._target;
                    _dt.changeColor(node.getColoringGroup(_selection._level),
                                Color.black);
                    _dt.highlight(node);
                    if (node == _node && _level.equals(_selection._level)) {
                        _deselect = true;
                        _selection.setTarget(null, null);
                    }
                    // _selection.removeTarget();
                }
                if (! _deselect) {
                    _selection.setTarget(_node, _level);
                    _dt.changeColor(_node.getColoringGroup(_selection._level),
                                _dt.getSelectColor());
                    _deselect = true;
                }
            }
        }
    }

    private DTMouseEventListener getSelectListener() {
        return new SelectListener();
    }
    /**
     * Used for text change events on a graphical element of the dependency
     * tree.
     *
     * @author 3zimmer
     *
     */
    protected class ChangeTextListener extends DTMouseEventListener {
        public void act(DOMMouseEvent devt) {
            // if left mouse button was pressed
            if (devt.getButton() == 0) {
                notifySelection(new PopupEvent(this, devt, _type, _node, _level));
            }
        }
    }

    protected DTMouseEventListener getChangeTextListener() {
        return new ChangeTextListener();
    }

    /**
     * Used for link reconnection events on a graphical element of the
     * dependency
     * tree.
     *
     * @author 3zimmer
     *
     */
    protected class ConnectListener extends DTMouseEventListener {
        public void act(DOMMouseEvent devt) {
            if (_selection._hasTarget && devt.getButton() == 2) {
                DepTreeNode oldNode = _selection._target;
                String oldLevel = _selection._level;
                String currentLevel = _dt.getCurrentLevel();
                if (oldLevel.equals(currentLevel)
                            && ! _node.getWhoLinks()
                                        .contains(oldNode.getIndex())) {
                    int newLink = - 1;
                    if (_node.getIndex() != oldNode.getIndex()) {
                        newLink = _node.getIndex();
                    }
                    oldNode.setLink(oldLevel, newLink);
                    _dt.getDecParse().redirectEdge(oldNode.getIndex(),
                                oldLevel,
                                newLink);
                    redraw(oldNode, oldLevel, true);
                }
                else if ("SYN".equals(currentLevel)
                            && ("DEPTREEWORD".equals(oldLevel) || "REF".equals(oldLevel))
                            && oldNode.getLink("REF") != _node.getIndex()) {
                    oldNode.setLink("REF", _node.getIndex());
                    int refTarget = _node.getIndex();
                    if (_node.getIndex() == oldNode.getIndex()) {
                        refTarget = - 1;
                        _selection._level = "DEPTREEWORD";
                        oldLevel = "DEPTREEWORD";
                    }
                    else {
                        _selection._level = "REF";
                        oldLevel = "REF";
                    }
                    _dt.getDecParse().redirectEdge(oldNode.getIndex(),
                                "REF",
                                refTarget);
                    redraw(oldNode, oldLevel, true);
                }
            }
        }
    }

    private DTMouseEventListener getConnectListener() {
        return new ConnectListener();
    }

    /**
     * Used for the field _selection containing the currently selected node and
     * level
     *
     * @author 3zimmer
     *
     */
    public class NodeSelection {
        boolean _hasTarget = false;
        DepTreeNode _target = null;
        String _level = null;

        /**
         *
         * @return true if a node is currently selected
         */
        public boolean hasTarget() {
            return _hasTarget;
        }

        public void setHasTarget(boolean hasTarget) {_hasTarget = hasTarget;}

        /**
         * Sets the selection to a node and level
         *
         * @param target
         * @param level
         */
        public void setTarget(DepTreeNode target, String level) {
            _hasTarget = (target != null);
            _target = target;
            _level = level;
        }

        public void removeTarget() {
            _hasTarget = false;
            _target = null;
            _level = null;
        }
    }

    /**
     * Used after being attached to a graphical element of the dependency tree.
     * Registers events caused by clicking on it.
     *
     * @author 3zimmer
     *
     */
    public abstract static class DTMouseEventListener implements EventListener {
        protected DepTreeNode _node;
        protected String _level;
        Com _type;

        public void handleEvent(org.w3c.dom.events.Event evt) {
            if (evt.getClass().equals(DOMMouseEvent.class)) {
                act((DOMMouseEvent) evt);
            }
        }

        public abstract void act(DOMMouseEvent devt);

        public void setFields(DepTreeNode node, String level, Com type) {
            _node = node;
            _level = level;
            _type = type;
        }
    }
}
