package nl.wernerkroneman.Drawy;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * User interface element resembling a chat window that allows
 * textual interaction with the user.
 */
public class UserInteractor extends JPanel implements ActionListener {

    private final JButton sendButton;
    private final JTextField textEntryField;
    private final JTextPane conversationView;
    private java.util.List<UserInteractorListener> listeners = new ArrayList<>();

    UserInteractor() {
        super();

        this.setMinimumSize(new Dimension(400, 100));

        this.setLayout(new GridBagLayout());

        // Conversation view is a big text pane
        GridBagConstraints c = new GridBagConstraints();

        c.gridx = 0;
        c.gridy = 0;
        c.gridwidth = 2; // 2-wide (above both the text field and the button
        c.weightx = 1;
        c.fill = GridBagConstraints.HORIZONTAL;

        conversationView = new JTextPane();
        conversationView.setPreferredSize(new Dimension(300, 100));
        conversationView.setEditable(false);

        JScrollPane scrollPane = new JScrollPane(conversationView);
        scrollPane.setPreferredSize(new Dimension(300, 100));

        this.add(scrollPane, c);

        // Input entry field

        GridBagConstraints cf = new GridBagConstraints();
        cf.gridx = 0;
        cf.gridy = 1;
        cf.weightx = 1;
        cf.weighty = 1;
        cf.fill = GridBagConstraints.HORIZONTAL;

        textEntryField = new JTextField();
        this.add(textEntryField, cf);
        textEntryField.addActionListener(this);

        // Submit button

        GridBagConstraints cd = new GridBagConstraints();
        cd.gridx = 1;
        cd.gridy = 1;
        cd.fill = GridBagConstraints.HORIZONTAL;

        sendButton = new JButton("Send");
        this.add(sendButton, cd);

        sendButton.addActionListener(this);
    }

    public void putText(String text) {
        conversationView.setText(conversationView.getText() + text + "\n");

    }

    public void addListener(UserInteractorListener listener) {
        this.listeners.add(listener);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        if (e.getSource() == sendButton || e.getSource() == textEntryField) {

            putText(textEntryField.getText() + "\n");

            for (Iterator<UserInteractorListener> iterator = listeners.iterator(); iterator.hasNext(); ) {
                UserInteractorListener listener = iterator.next();

                if (!listener.textEntered(textEntryField.getText())) {
                    iterator.remove();
                }
            }
            textEntryField.setText("");
        }
    }

    public interface UserInteractorListener {
        /**
         * Called when the user submits text, this will always be on
         * the GUI thread.
         * <p>
         * Return value determines whether the listener remains open,
         * this is useful for one-shot listeners.
         *
         * @param input The submitted text
         * @return Whether the listener should be kept in the list of listeners.
         */
        boolean textEntered(String input);
    }
}
