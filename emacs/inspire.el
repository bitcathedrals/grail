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
                           "I was throwing straw-men at a troll to see the flames shoot out"
                           "Larry, your killing your father!"
                           "Smokey, you\'re entering a world of pain... a world of pain."
                           "AK-47, When you absolutely positively have to kill every motherfucker in the room, accept no substitutes."
                           "Satan is in my house..."
                           "Never go full retard!"
                           "I don\'t remember asking you a GODDAMN thing."
                           "Scout Master Kevin?"
                           "Go home! You smell like someone shit in a civil war wound that had become gangrenous"
                           "All you motherfuckers are going to pay! you are the one\'s who are the ball lickers"
                           "I don\'t negotiate pumpkin fucker."
                           "I can\'t wait to never speak of this. As soon as possible."
                           "What am I going to do now? Probably something terrible, knowing you."
                           "Prison wallet? I hope I don\'t know what your saying."
                           "What\'s your super power? Cultural appropriation?"
                           "You're so dark! Are you sure your not from the D.C Universe?"
                           "It\s not who you are underneath, it\'s what you do."
                           "I will splash the pot whenever I want to splash the pot!"))

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

