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

import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader

internal object SyntaxNetLink {

    fun parse(english: String): PhraseTree {

        if (!english.all({
            it in 'a'..'z' ||
                    it in 'A'..'Z' ||
                    it in '0'..'9' ||
                    it == '.' ||
                    it == ',' ||
                    it == ' '
        })) {
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