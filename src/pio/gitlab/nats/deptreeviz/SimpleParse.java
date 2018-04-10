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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * A class that only implements the basics for data obtained from conll files
 * Created by Arne Köhn on 14.02.17.
 */
public class SimpleParse implements ParseInterface<SimpleWord> {
    private List<SimpleWord> words;
    private List<String> levels;
    private Map<String, List<Integer>> structure;
    private Map<String, List<String>> labels;

    public SimpleParse(List<SimpleWord> words, List<Integer> heads, List<String> labels) {
        levels = new ArrayList<>();
        levels.add("SYN");
        this.words = words;
        structure = new HashMap<>();
        structure.put("SYN", heads);
        this.labels = new HashMap<>();
        this.labels.put("SYN", labels);
    }

    @Override
    public String print() {
        return null;
    }

    @Override
    public List<SimpleWord> getWords() {
        return words;
    }

    @Override
    public List<String> getLevels() {
        return levels;
    }

    @Override
    public Map<String, List<Integer>> getVerticesStructure() {
        return structure;
    }

    @Override
    public Map<String, List<String>> getVerticesLabels() {
        return labels;
    }

    @Override
    public void redirectEdge(int pos, String level, int to) {

    }

    /**
     * Create parse object from conll string.
     * @param conll List of CoNLL lines
     */
    public static SimpleParse fromConll(List<String> conll) {
        List<Integer> heads = new ArrayList<>();
        List<String> labels = new ArrayList<>();
        List<SimpleWord> words = new ArrayList<>();
        for (String line : conll) {
            String[] args = line.split("\t");
            if (args.length < 8)
                break;
            heads.add(Integer.parseInt(args[6])-1);
            labels.add(args[7]);
            words.add(new SimpleWord(args[1], args[4]));
        }
        return new SimpleParse(words, heads, labels);
    }
}
