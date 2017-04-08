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

import nl.wernerkroneman.Drawy.ModelEditor.Interpreters.*
import nl.wernerkroneman.Drawy.Modelling.GroupModel
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.Companion.ABOVE
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.Companion.BELOW
import nl.wernerkroneman.Drawy.Modelling.RelativeSize
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PatternInterpreter
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhrasePatternBuilder
import nl.wernerkroneman.Drawy.ParseTreeMatcher.buildPattern
import java.util.*

fun createDefaultModelInterpreter(knowledge: Knowledge = Knowledge.knowledgeWithPrimitives()):
        PatternInterpreter {

    val interpreter = PatternInterpreter()

    val modelInterpreter = ModelInterpreter(knowledge, interpreter)

    val createCommandInterpreter = CreateCommandInterpreter(interpreter)

    val distanceInterpreter = DistanceInterpreter()

    interpreter.addPattern(createCommandInterpreter,
            PhrasePatternBuilder()
                    .role("ROOT")
                    .nature("NN")
                    .name("what")
                    .create())

    interpreter.addPattern(modelInterpreter,
            PhrasePatternBuilder()
                    .nature("NN")
                    .name("name")
                    .create())

    interpreter.addPattern(createCommandInterpreter,
            PhrasePatternBuilder()
                    .word("Give")
                    .child(PhrasePatternBuilder()
                            .word("me")
                            .create())
                    .child(PhrasePatternBuilder()
                            .name("what")
                            .create())
                    .child(PhrasePatternBuilder()
                            .role("punct")
                            .create()
                    ).create())

    interpreter.addPattern(createCommandInterpreter,
            PhrasePatternBuilder()
                    .word("Create")
                    .child(PhrasePatternBuilder()
                            .name("what")
                            .create())
                    .child(buildPattern {
                        name("where")
                        role("prep")
                        optional()
                    })
                    .child(PhrasePatternBuilder()
                            .role("punct")
                            .optional()
                            .create()
                    ).create())

    interpreter.addPattern(RelativePositionInterpreter(ABOVE, interpreter),
            PhrasePatternBuilder()
                    .role("prep")
                    .word("above")
                    .children(
                            buildPattern {
                                role("npadvmod")
                                name("distance")
                                optional()},
                            PhrasePatternBuilder()
                                    .role("pobj")
                                    .name("relative_to")
                                    .create()
                    ).create())

    interpreter.addPattern(RelativePositionInterpreter(BELOW, interpreter),
            PhrasePatternBuilder()
                    .role("prep")
                    .word("below")
                    .children(
                            PhrasePatternBuilder()
                                    .role("pobj")
                                    .name("relative_to")
                                    .create()
                    ).create())

    interpreter.addPattern(distanceInterpreter,
            buildPattern {
                nature("NN*")
                child( buildPattern {
                            nature("CD")
                            role("num")
                            name("amount")
                })
            })

    interpreter.addPattern(constantInterpreter(GroupModel.ComponentDesignator.RelativeComponent(-1)),
            buildPattern {
                word("other")
                child { word("each")}
            })

    interpreter.addPattern(constantInterpreter(GroupModel.ComponentDesignator.RelativeComponent(-1)),
            buildPattern {
                word("one")
                child { word("another")}
            })

    interpreter.addPattern(constantInterpreter(RelativeSize(1.5)),
            buildPattern { word("big")})

    interpreter.addPattern(constantInterpreter(RelativeSize(1/1.5)),
            buildPattern { word("small")})

    interpreter.addPattern(NumberInterpreter(),
            buildPattern { word("[0-9]+"); nature("CD"); role("num"); name("number") })

    interpreter.addPattern(LastCreatedComponentInterpreter(),
            buildPattern { word("that"); role("pobj") })

    interpreter.addPattern(LastCreatedComponentInterpreter(),
            buildPattern {
                role("pobj"); name("specifier"); child {
                word("that"); role("det")
            }
            })

    return interpreter

}

/**
 * Marker interface for interpretation contexts
 */
interface InterpretationContext

/**
 * Get an Iterable describing all previous commands, last-to-first
 */
private val EditorCommand.previousCommands: Iterable<EditorCommand>
    get() {
        val firstInHistory = this

        return object : Iterable<EditorCommand> {

            override fun iterator(): Iterator<EditorCommand> {
                return object : Iterator<EditorCommand> {

                    var current = firstInHistory

                    override fun hasNext(): Boolean {
                        return current.previous != null
                    }

                    override fun next(): EditorCommand {
                        current = current.previous ?:
                                throw NoSuchElementException("No more history.")
                        return current
                    }

                }
            }

        }
    }
