package nl.wernerkroneman.Drawy.ModelEditor

import javafx.util.Pair
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree

import java.io.StringReader
import java.util.Scanner
import java.util.Stack

fun parsePhraseTree(parserOutput: String) : PhraseTree {

    var rootWord :PhraseTree? = null

    val parseStack = Stack<PhraseTree>()

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

        val part = PhraseTree(tokens[0], tokens[1], tokens[2])

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
            parseStack.peek().addChild(part)
        }

        // Push it onto the stack so we may attach children to it if necessary
        parseStack.push(part)

    }

    if (rootWord == null) {
        throw RuntimeException("Parse error")
    } else {
        return rootWord
    }

}
/*
fun toString(phrase: PhraseTree): String {
    val strstr = StringBuilder()

    val printStack = Stack<Pair<PhraseTree, Int>>()

    printStack.push((rootWord, 0))

    while (!printStack.empty()) {
        if (strstr.length > 0) {
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

