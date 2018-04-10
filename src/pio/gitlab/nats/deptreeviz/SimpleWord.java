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

/**
 * A class that only implements the basics for data obtained from conll files
 * Created by Arne Köhn on 14.02.17.
 */
public class SimpleWord implements WordInterface {
    private String word;
    private String cat;

    public SimpleWord(String word, String cat) {
        this.word = word;
        this.cat = cat;
    }

    @Override
    public String getWord() {
        return word;
    }

    @Override
    public String getFeature(String featureName) {
        if ("cat".equals(featureName))
            return cat;
        return "";
    }
}
