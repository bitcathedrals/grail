(setq
  words-of-encouragement '("The name is Bond. James Bond."
                           "These are your father's parentheses. Elegant weapons from a more civilized age."
                           "We were on the edge of the desert when the Emacs took hold."
                           "What are you? some one man GPS? Were lost! we are really fucking lost!"
                           "Do you see Larry? Do you see what happens when you fuck a stranger in the ass!"
                           "It's a fucking show dog with fucking papers"
                           "Smokey this isn't \'Nam, there are rules"
                           "He's a Nihlist. That must be exhausting."
                           "Fair! who\'s the Nihlist now?"
                           "This is like having a Pine Cone shoved up my ass"
                           "Is this your homework Larry?"
                           "No Walter, I did not think he was about to crack"
                           "This is Flaming Dragon!"
                           "Would you give me a foot massage? Fuck You!"
                           "You will name is the Lord! When I lay my vengeance upon you!"
                           "You don\'t care about money because you have it."
                           "You\'ve have been missing alot of work. I wouldn\' say I\'ve been missing it."
                           "He\'s a straight shooter with upper management written all over him."
                           "In the beginning was the lambda, and the lambda was with Emacs, and Emacs was the lambda."
                           "Producing crap only takes a squeeze"
                           "I\'m using Linux. A library Emacs uses to communicate with hardware."
                           "I was throwing straw-men at a troll to see the flames shoot out"))

(defun get-inspiration ()
  (nth (random (length words-of-encouragement)) words-of-encouragement))

(defun inspire ()
  "inspire

   I\'m the funny man! so fucking funny!"
  (interactive)
  (message (get-inspiration)) )

(defun add-inspire-to-status ()
  (buffer-status-add (get-inspiration)) )

(add-hook 'after-change-major-mode-hook 'add-inspire-to-status)

(provide 'profile/inspire)

