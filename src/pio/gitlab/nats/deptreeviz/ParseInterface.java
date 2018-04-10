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

import java.util.List;
import java.util.Map;

/**
 * A general interface for everything that represents a possibly multi-leveled
 * dependency tree / graph
 * Created by Arne Köhn on 08.02.17.
 */
public interface ParseInterface<T extends WordInterface> {
    String print();

    List<T> getWords();

    List<String> getLevels();

    Map<String, List<Integer>> getVerticesStructure();

    Map<String, List<String>> getVerticesLabels();

    void redirectEdge(int pos, String level, int to);
}

