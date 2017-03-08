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

package nl.wernerkroneman.Drawy.Interface

import java.awt.Dimension
import java.awt.GridBagConstraints
import java.awt.GridBagLayout
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.util.*
import javax.swing.*

/**
 * User interface element resembling a chat window that allows
 * textual interaction with the user.
 */
class UserInteractor internal constructor() : JPanel(), ActionListener {

    private val sendButton: JButton
    private val textEntryField: JTextField
    private val conversationView: JTextPane
    private val listeners = ArrayList<(String) -> Boolean>()

    init {

        this.minimumSize = Dimension(400, 100)

        this.layout = GridBagLayout()

        // Conversation view is a big text pane
        val c = GridBagConstraints()

        c.gridx = 0
        c.gridy = 0
        c.gridwidth = 2 // 2-wide (above both the text field and the button
        c.weightx = 1.0
        c.fill = GridBagConstraints.HORIZONTAL

        conversationView = JTextPane()
        conversationView.preferredSize = Dimension(300, 100)
        conversationView.isEditable = false

        val scrollPane = JScrollPane(conversationView)
        scrollPane.preferredSize = Dimension(300, 100)

        this.add(scrollPane, c)

        // Input entry field

        val cf = GridBagConstraints()
        cf.gridx = 0
        cf.gridy = 1
        cf.weightx = 1.0
        cf.weighty = 1.0
        cf.fill = GridBagConstraints.HORIZONTAL

        textEntryField = JTextField()
        this.add(textEntryField, cf)
        textEntryField.addActionListener(this)

        // Submit button

        val cd = GridBagConstraints()
        cd.gridx = 1
        cd.gridy = 1
        cd.fill = GridBagConstraints.HORIZONTAL

        sendButton = JButton("Send")
        this.add(sendButton, cd)

        sendButton.addActionListener(this)
    }

    fun putText(text: String) {
        conversationView.text = conversationView.text + text + "\n"
    }

    fun addListener(listener: (String) -> Boolean) {
        this.listeners.add(listener)
    }

    override fun actionPerformed(e: ActionEvent) {
        if (e.source === sendButton || e.source === textEntryField) {

            putText(textEntryField.text + "\n")

            val iterator = listeners.iterator()
            while (iterator.hasNext()) {
                val listener = iterator.next()

                if (!listener(textEntryField.text)) {
                    iterator.remove()
                }
            }
            textEntryField.text = ""
        }
    }
}
