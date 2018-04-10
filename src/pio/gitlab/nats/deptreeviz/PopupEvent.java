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

import java.util.EventObject;


/**
 * Used for signaling that a menu is to pop up after clicking on a graphical
 * element of the dependency tree.
 *
 * @author 3zimmer
 *
 */
public class PopupEvent extends EventObject {
    private static final long serialVersionUID = 1L;
    private DepTreeNode _node;
    private String _level;
    private DepTreeBaseInteractor.Com _type;
    private DOMMouseEvent _devt;

    public PopupEvent(
            Object source,
            DOMMouseEvent devt,
            DepTreeBaseInteractor.Com type,
            DepTreeNode node,
            String level) {
        super(source);
        _node = node;
        _level = level;
        _type = type;
        _devt = devt;
    }

    public DepTreeNode getNode() {
        return _node;
    }

    public String getLevel() {
        return _level;
    }

    public DepTreeBaseInteractor.Com getType() {
        return _type;
    }

    public DOMMouseEvent getDevt() {
        return _devt;
    }


}