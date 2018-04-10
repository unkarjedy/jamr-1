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

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Example class that converts Conll to SVG
 * @author Arne Köhn
 */
class ConllToSVG {
	public static void main(String[] args) throws IOException {
		List<String> conll;
		Writer w;;
		if (args.length == 2) {
			conll = Files.readAllLines(Paths.get(args[0]));
			w = new FileWriter(new File(args[1]));
		} else {
			BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
			conll = br.lines().collect(Collectors.toList());
			w = new PrintWriter(System.out);
		}

		SimpleParse p = SimpleParse.fromConll(conll);
		DepTree<SimpleParse, SimpleWord> dt = new DepTree<>(p);
		// write tree
		dt.writeTree(w);
	}
}
