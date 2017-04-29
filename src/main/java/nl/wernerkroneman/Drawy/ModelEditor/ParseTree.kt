/*
 * Copyright (c) 2017 Werner Kroneman
 *
 * This file is part of DrawyMcDrawface.
 *
 * DrawyMcDrawface is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * DrawyMcDrawface is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with DrawyMcDrawface.  If not, see <http://www.gnu.org/licenses/>.
 */

package nl.wernerkroneman.Drawy.ModelEditor

import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree
import java.io.StringReader
import java.util.*

class PhraseTreeBuilder(var word: String,
                        var nature: String,
                        var role: String,
                        var children: MutableList<PhraseTreeBuilder>) {

    fun compile(): PhraseTree {

        return PhraseTree(word, nature, role,
                children.map { it.compile() })

    }
}

/**
 * Turn the output of Parsey into a PhraseTree object.
 */
fun parsePhraseTree(parserOutput: String) : PhraseTree {

    var rootWord: PhraseTreeBuilder? = null

    val parseStack = Stack<PhraseTreeBuilder>()

    val strstr = Scanner(StringReader(parserOutput))

    while (strstr.hasNextLine()) {
        var line = strstr.nextLine()

        // Find how far away the + is (if there is any)
        val plus = line.indexOf('+')

        // Calculate the depth based on + position
        val depth = if (plus == -1) 0 else (plus + 4) / 4

        if (plus > 0) {
            // Cut off up until "+--" included
            line = line.substring(plus + 4)
        }

        // Split up the remainder into word/nature/role
        val tokens = line.split(" ".toRegex()).dropLastWhile { it.isEmpty() }.toTypedArray()

        val part = PhraseTreeBuilder(tokens[0], tokens[1], tokens[2], mutableListOf())

        if (depth == 0) {
            // This is the root
            rootWord = part
        } else {
            // Pop until we have the right number of ancestors
            while (depth < parseStack.size) {
                parseStack.pop()
            }

            // Quick sanity check
            assert(!parseStack.empty())

            // Sentence part at the top is now parent
            parseStack.peek().children.add(part)
        }

        // Push it onto the stack so we may attach children to it if necessary
        parseStack.push(part)

    }

    if (rootWord == null) {
        throw RuntimeException("Parse error")
    } else {
        return rootWord.compile()
    }

}
/*
fun toString(phrase: PhraseTree): String {
    val strstr = StringBuilder()

    val printStack = Stack<Pair<PhraseTree, Int>>()

    printStack.push((rootWord, 0))

    while (!printStack.empty()) {
        if (strstr.x > 0) {
            // Not the first line, prepend a newline
            strstr.append('\n')
        }

        val word = printStack.peek().key
        val depth = printStack.peek().value

        printStack.pop()

        for (i in 0..depth - 1 - 1) {
            strstr.append("    ")
        }

        if (depth > 0) {
            strstr.append(" +-- ")
        }

        strstr.append(word.rootWord + " " + word.nature + " " + word.role)

        val children = word.children
        for (i in children.indices.reversed()) {
            val child = children[i]
            printStack.push(Pair(child, depth + 1))
        }
    }

    return strstr.toString()
}
}*/

