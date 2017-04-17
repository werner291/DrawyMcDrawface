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

import nl.wernerkroneman.Drawy.ModelEditor.Commands.EditorCommand
import nl.wernerkroneman.Drawy.ModelEditor.Interpreters.*
import nl.wernerkroneman.Drawy.Modelling.AbsoluteScalar
import nl.wernerkroneman.Drawy.Modelling.Distance
import nl.wernerkroneman.Drawy.Modelling.FixedDistance
import nl.wernerkroneman.Drawy.Modelling.GroupModel
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.Companion.ABOVE
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PatternInterpreter
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhrasePatternBuilder
import nl.wernerkroneman.Drawy.ParseTreeMatcher.buildPattern
import java.util.*

fun createDefaultModelInterpreter(knowledge: Knowledge = Knowledge.knowledgeWithPrimitives()):
        PatternInterpreter {

    val interpreter = PatternInterpreter()

    val modelInterpreter = ModelInstanceInterpreter(knowledge, interpreter)

    val createCommandInterpreter = CreateCommandInterpreter(interpreter)

    val distanceInterpreter = DistanceInterpreter()

    interpreter.addPattern(createCommandInterpreter,
            PhrasePatternBuilder()
                    .role("ROOT")
                    .nature("NN")
                    .name("what")
                    .child { word("a"); role("det") }
                    .child { repeat(0, null) }
                    .create())

    interpreter.addPattern(modelInterpreter,
            PhrasePatternBuilder()
                    .nature("NN")
                    .name("name")
                    .child { word("a"); role("det") }
                    .child { repeat(0, null) }
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
                    .word("(?:Create)|(?:Put)")
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

    interpreter.addPattern(createCommandInterpreter,
            buildPattern {
                word("Stick")
                child {
                    nature("NN")
                    name("what")
                }
                child {
                    word("into")
                    role("prep")
                    name("where")
                }
                child {
                    role("punct")
                    optional()
                }
            })

    interpreter.addPattern(RelativePositionInterpreter(ABOVE, Distance.ANY, interpreter),
            buildPattern {
                role("prep")
                word("above")
                child {
                    role("npadvmod")
                    name("distance")
                    optional()
                }
                child {
                    role("pobj")
                    name("relative_to")
                }
            })

    interpreter.addPattern(RelativePositionInterpreter(ABOVE, FixedDistance(0.0), interpreter),
            buildPattern {
                role("prep")
                word("on")
                child {
                    word("top")
                    child {
                        word("of")
                        child {
                            role("pobj")
                            name("relative_to")
                        }
                    }
                }
            })

    interpreter.addPattern(RelativePositionInterpreter(ABOVE, FixedDistance(-0.1), interpreter),
            buildPattern {
                role("prep")
                word("into")
                child {
                    word("front")
                    child { word("the") }
                    child {
                        word("of")
                        child {
                            nature("NN")
                            name("relative_to")
                        }
                    }
                }
            })

    interpreter.addPattern(distanceInterpreter,
            buildPattern {
                nature("NN*")
                child(buildPattern {
                    nature("CD")
                    role("num")
                    name("amount")
                })
            })

    interpreter.addPattern(constantInterpreter(GroupModel.ComponentDesignator.RelativeComponent(-1)),
            buildPattern {
                word("other")
                child { word("each") }
            })

    interpreter.addPattern(constantInterpreter(GroupModel.ComponentDesignator.RelativeComponent(-1)),
            buildPattern {
                word("one")
                child { word("another") }
            })

    interpreter.addPattern(RelativeSizeInterpreter(AbsoluteScalar(1.5)),
            buildPattern { word("big") })

    interpreter.addPattern(RelativeSizeInterpreter(AbsoluteScalar(0.5)),
            buildPattern { word("small") })

    interpreter.addPattern(SizeRelativeToLastCreatedInterpreter(), buildPattern { word("smaller") })

    interpreter.addPattern(NumberInterpreter(),
            buildPattern { word("[0-9]+"); nature("CD"); role("num"); name("number") })

    interpreter.addPattern(LastCreatedComponentInterpreter(),
            buildPattern { word("(?:that)|(?:it)"); role("pobj") })

    interpreter.addPattern(LastCreatedComponentInterpreter(),
            buildPattern { word("this"); role("nsubj") })

    interpreter.addPattern(FindModelInterpreter(),
            buildPattern {
                nature("NN")
                name("name")
                child { word("(?:that)|(?:the)"); role("det") }
                child { repeat(0, null) }
            })

    interpreter.addPattern(RenameCommandInterpreter(interpreter),
            buildPattern {
                role("ROOT")
                nature("NN")
                name("new name")
                child {
                    role("nsubj")
                    name("target")
                }
                child {
                    word("is")
                }
                child {
                    repeatAny()
                }
            })

    return interpreter

}


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
