package nl.wernerkroneman.Drawy.ModelEditor

import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree

import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader

internal object SyntaxNetLink {

    fun parse(english: String): PhraseTree {

        if (!english.all({it in 'a'..'z' || it in 'A'..'Z' || it == '.' || it == ',' || it == ' '})) {
            throw RuntimeException("String contains illegal or unsafe characters: " + english)
        }

        // TODO isn't it beautiful?
        try {

            val processBuilder = ProcessBuilder("/bin/sh", "-c", "echo '" + english + "' | docker run " +
                    "--rm -i brianlow/syntaxnet")

            val p = processBuilder.start()

            p.waitFor()

            BufferedReader(InputStreamReader(p.inputStream)).use { output ->
                var line: String

                var skipLines = 2

                // Read the rest of the stream into a string
                val str = StringBuilder("")

                output.forEachLine {
                    if (skipLines-- <= 0) {
                        str.append(it)
                        str.append('\n')
                    }
                }

                // Parse and return
                val parserOutput = str.toString()

                System.err.println(parserOutput)

                return parsePhraseTree(parserOutput)
            }

        } catch (e: IOException) {
            e.printStackTrace()
        } catch (e: InterruptedException) {
            e.printStackTrace()
        }

        throw RuntimeException("Something went wrong.")
    }

}