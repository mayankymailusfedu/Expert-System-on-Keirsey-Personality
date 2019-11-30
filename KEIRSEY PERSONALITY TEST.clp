;*********************************************************************************************
;Mayank Yadav - U94319493
;
;Keirsey Personality Traits - This system will predict the traits that fall under user's Personality
;and will also suggest how to master those traits
;
;Temperament is a configuration of observable personality traits, 
;such as habits of communication, patterns of action, and sets of characteristic attitudes, 
;values, and talents. It also encompasses personal needs, the kinds of contributions that 
;individuals make in the workplace, and the roles they play in society. Each temperament has 
;its own unique qualities and shortcomings, strengths and challenges. Dr. David Keirsey has 
;identified humankind's four basic temperaments as the Artisan, the Guardian, the Idealist, 
;and the Rational.
;
;Running the program - 
;(clear)
;Load the file
;(reset)
;(run)
;
;Ref - https://keirsey.com/temperament-overview/
;********************************************************************************************

;*******
;Classes
;*******
;Class for storing User's Name
(defclass PERSON (is-a USER) 
        (slot person_name)
        (slot identified_name)
        (role concrete) 
        )

;Class for storing User's Personality
(defclass TEMPERAMENT (is-a USER)
        (slot pre_type_one)
        (slot pre_type_two)
        (slot main_type)
        (slot sub_type)
        (slot personalities)
        (slot suggestions)
        (role concrete) 
        )

;*********
;Instances
;*********
(definstances PERSON-INSTANCES 
         (mayank of PERSON)
         )

(definstances TEMPERAMENT-INSTANCES 
         (temp of TEMPERAMENT)
         )

;*******************
;Ask Questions Rules
;*******************
(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))


;**********************
;Program Starting Point
;**********************

;Ask the user's name
(defrule ask-person-name  (declare (salience 30))
    ?ins <- (object (is-a PERSON) (person_name nil))
    =>
    (printout t "Enter your name: " )
    (send ?ins put-person_name (read))
    (send ?ins put-identified_name yes)
    )

;Introduce the System to User
(defrule write-person-name  (declare (salience 29))
    ?ins <- (object (is-a PERSON) (identified_name yes))
    =>
    (printout t "Welcome to Keirsey Personality Test ")
    (printout t (send ?ins get-person_name) crlf)
    (printout t "This system will predict the traits that fall under your Personality" crlf)
    (printout t "And will also suggest how to master those traits" crlf)
    (printout t "Now, please answer these Questions." crlf)
    (printout t crlf)
    )

;Ask if the person is utilitarian
(defrule ask-utilitarian (declare (salience 28))
    ?ins <- (object (is-a TEMPERAMENT) (pre_type_one nil))
    (not (utilitarianCount ?))
    =>
    (assert (utilitarian (yes-or-no-p "If you are doing an academic assignment & there is an optional task, will you do it (yes/no)? ")))
    (assert (utilitarianCount no))
    )

;Verify if the person is utilitarian
(defrule verify-utilitarian (declare (salience 27))
    (utilitarian yes)
    =>
    (send [temp] put-pre_type_one utilitarian)
    (assert (utilitarianCount yes))
    )

;Ask if the person is cooperative
(defrule ask-cooperative (declare (salience 26))
    ?ins <- (object (is-a TEMPERAMENT) (pre_type_one nil))
    (not (cooperativeCount ?))
    (utilitarianCount no)
    =>
    (assert (cooperative (yes-or-no-p "If your friend ask for your assignment, do you share your assignment with them (yes/no)? ")))
    (assert (cooperativeCount no))
    )

;Verify if the person is cooperative
(defrule verify-cooperative (declare (salience 25))
    (cooperative yes)
    =>
    (send [temp] put-pre_type_one cooperative)
    (assert (cooperativeCount yes))
    )

;Insert value for pre_type_one
(defrule verify-pre_type_one (declare (salience 24))
    ?ins <- (object (is-a TEMPERAMENT) (pre_type_one nil))
    (cooperativeCount no)
    (utilitarianCount no)
    =>
    (send ?ins put-pre_type_one utilitarian)
    )

;Ask if the person is concrete
(defrule ask-concrete (declare (salience 23))
    ?ins <- (object (is-a TEMPERAMENT) (pre_type_two nil))
    (not (concreteCount ?))
    =>
    (assert (concrete (yes-or-no-p "While waiting in a queue, do you talk to other's reagarding daily news (yes/no)? ")))
    (assert (concreteCount no))
    )

;Verify if the person is concrete
(defrule verify-concrete (declare (salience 22))
    (concrete yes)
    =>
    (send [temp] put-pre_type_two concrete)
    (assert (concreteCount yes))
    )

;Ask if the person is abstract
(defrule ask-abstract (declare (salience 21))
    ?ins <- (object (is-a TEMPERAMENT) (pre_type_two nil))
    (not (abstractCount ?))
    (concreteCount no)
    =>
    (assert (abstract (yes-or-no-p "While talking to your friends, do you talk about your future (yes/no)? ")))
    (assert (abstractCount no))
    )

;Verify if the person is abstract
(defrule verify-abstract (declare (salience 20))
    (abstract yes)
    =>
    (send [temp] put-pre_type_two abstract)
    (assert (abstractCount yes))
    )

;Insert value for pre_type_two
(defrule verify-pre_type_two (declare (salience 19))
    ?ins <- (object (is-a TEMPERAMENT) (pre_type_two nil))
    (concreteCount no)
    (abstractCount no)
    =>
    (send ?ins put-pre_type_two concrete)
    )

;***********************************************
;Intermediate Conclusion - 1 (Finding Main Type)
;***********************************************

;*********************************
;Guardian = Concrete + Cooperative
;*********************************
(defrule check-guardian (declare (salience 18))
    ?ins <- (object (is-a TEMPERAMENT) (pre_type_one cooperative) (pre_type_two concrete))
    =>
    (send ?ins put-main_type guardian) 
    )

;********************************
;Artsian = Concrete + Utilitarian
;********************************
(defrule check-artisan (declare (salience 17))
    ?ins <- (object (is-a TEMPERAMENT) (pre_type_one utilitarian) (pre_type_two concrete))
    =>
    (send ?ins put-main_type artisan) 
    )

;*********************************
;Idealist = Abstract + Cooperative
;*********************************
(defrule check-idealist (declare (salience 16))
    ?ins <- (object (is-a TEMPERAMENT) (pre_type_one cooperative) (pre_type_two abstract))
    =>
    (send ?ins put-main_type idealist) 
    )

;*********************************
;Rational = Abstract + Utilitarian
;*********************************
(defrule check-rational (declare (salience 15))
    ?ins <- (object (is-a TEMPERAMENT) (pre_type_one utilitarian) (pre_type_two abstract))
    =>
    (send ?ins put-main_type rational) 
    )

;**********************************************
;Intermediate Conclusion - 2 (Finding Sub Type)
;**********************************************

;Check if the person belong to which category of Artisan
(defrule further-artisan (declare (salience 14))
    ?ins <- (object (is-a TEMPERAMENT) (main_type artisan))
    (not (artisan_flag ?))
    =>
    (assert (promoter (yes-or-no-p "Do you invest in Stock (yes/no)? ")))
    (assert (crafter (yes-or-no-p "Have you ever watched James Wan's The Conjuring alone at night (yes/no)? ")))
    (assert (performer (yes-or-no-p "Have you ever continued playing a game knowing that you are not gonna win (yes/no)? ")))
    (assert (composer (yes-or-no-p "Do you keep your room clean (yes/no)? ")))
    (assert (artisan_flag yes))
    )

(defrule artisan-promoter (declare (salience 13))
    ?ins <- (object (is-a TEMPERAMENT) (main_type artisan) (sub_type nil))
    (promoter yes)
    (artisan_flag yes)
    =>
    (send ?ins put-sub_type promoter)
    (send ?ins put-personalities magnetic_present_unpredictable_proactive_tactical)
    (assert (artisan_flag2 yes))
    )

(defrule artisan-crafter (declare (salience 12))
    ?ins <- (object (is-a TEMPERAMENT) (main_type artisan) (sub_type nil))
    (crafter yes)
    (artisan_flag yes)
    =>
    (send ?ins put-sub_type crafter)
    (send ?ins put-personalities audacious_camaraderie_coordinated_instinctive_freedom)
    (assert (artisan_flag2 yes))
    )

(defrule artisan-performer (declare (salience 11))
    ?ins <- (object (is-a TEMPERAMENT) (main_type artisan) (sub_type nil))
    (performer yes)
    (artisan_flag yes)
    =>
    (send ?ins put-sub_type performer)
    (send ?ins put-personalities engaging_generous_charming_sensational_immediate)
    (assert (artisan_flag2 yes))
    )

(defrule artisan-composer (declare (salience 10))
    ?ins <- (object (is-a TEMPERAMENT) (main_type artisan) (sub_type nil))
    (composer yes)
    (artisan_flag yes)
    =>
    (send ?ins put-sub_type composer)
    (send ?ins put-personalities sensory_absorbed_spontaneous_internal_kind)
    (assert (artisan_flag2 yes))
    )

(defrule artisan-sub_type-verify (declare (salience 9))
    ?ins <- (object (is-a TEMPERAMENT) (main_type artisan) (sub_type nil))
    (not (artisan_flag2 ?))
    =>
    (send ?ins put-sub_type composer)
    (send ?ins put-personalities sensory_absorbed_spontaneous_internal_kind)
    )

;Check if the person belong to which category of guardian
(defrule further-guardian (declare (salience 8))
    ?ins <- (object (is-a TEMPERAMENT) (main_type guardian))
    (not (guardian_flag ?))
    =>
    (assert (supervisor (yes-or-no-p "Do you think other people respect your decision (yes/no)? ")))
    (assert (inspector (yes-or-no-p "If you lose a toy, do you admit it's your fault  (yes/no)? ")))
    (assert (provider (yes-or-no-p "Do you use Facebook/Instagram/Twitter/SnapChat (yes/no)? ")))
    (assert (protector (yes-or-no-p "Do you have an artifact in your room that is atleast 10 years old (yes/no)? ")))
    (assert (guardian_flag yes))
    )

(defrule guardian-supervisor (declare (salience 7))
    ?ins <- (object (is-a TEMPERAMENT) (main_type guardian) (sub_type nil))
    (supervisor yes)
    (guardian_flag yes)
    =>
    (send ?ins put-sub_type supervisor)
    (send ?ins put-personalities responsible_structured_forceful_authorative_experienced)
    (assert (guardian_flag2 yes))
    )

(defrule guardian-inspector (declare (salience 6))
    ?ins <- (object (is-a TEMPERAMENT) (main_type guardian) (sub_type nil))
    (inspector yes)
    (guardian_flag yes)
    =>
    (send ?ins put-sub_type inspector)
    (send ?ins put-personalities reliable_institutional_inspecting_standardizing_conservative)
    (assert (guardian_flag2 yes))
    )

(defrule guardian-provider (declare (salience 5))
    ?ins <- (object (is-a TEMPERAMENT) (main_type guardian) (sub_type nil))
    (provider yes)
    (guardian_flag yes)
    =>
    (send ?ins put-sub_type provider)
    (send ?ins put-personalities integrous_providing_caring_procedural_collaborating)
    (assert (guardian_flag2 yes))
    )

(defrule guardian-protector (declare (salience 4))
    ?ins <- (object (is-a TEMPERAMENT) (main_type guardian) (sub_type nil))
    (protector yes)
    (guardian_flag yes)
    =>
    (send ?ins put-sub_type protector)
    (send ?ins put-personalities dedicated_selfssacrificing_thorough_respectful_stabilizing)
    (assert (guardian_flag2 yes))
    )

(defrule guardian-sub_type-verify (declare (salience 3))
    ?ins <- (object (is-a TEMPERAMENT) (main_type guardian) (sub_type nil))
    (not (guardian_flag2 ?))
    =>
    (send ?ins put-sub_type protector)
    (send ?ins put-personalities dedicated_selfssacrificing_thorough_respectful_stabilizing)
    )

;Check if the person belong to which category of idealist
(defrule further-idealist (declare (salience 2))
    ?ins <- (object (is-a TEMPERAMENT) (main_type idealist))
    (not (idealist_flag ?))
    =>
    (assert (teacher (yes-or-no-p "Do you use hand gestures while talking to people (yes/no)? ")))
    (assert (counselor (yes-or-no-p "Can you correctly predict your loved ones feeling before talking to them (yes/no)? ")))
    (assert (champion (yes-or-no-p "Do you like to explore a Jungle during your vacation (yes/no)? ")))
    (assert (healer (yes-or-no-p "Do you speak what's on your mind (yes/no)? ")))
    (assert (idealist_flag yes))
    )

(defrule idealist-teacher (declare (salience 1))
    ?ins <- (object (is-a TEMPERAMENT) (main_type idealist) (sub_type nil))
    (teacher yes)
    (idealist_flag yes)
    =>
    (send ?ins put-sub_type teacher)
    (send ?ins put-personalities intuitive_influence_interpersonal_principled)
    (assert (idealist_flag2 yes))
    )

(defrule idealist-counselor (declare (salience 0))
    ?ins <- (object (is-a TEMPERAMENT) (main_type idealist) (sub_type nil))
    (counselor yes)
    (idealist_flag yes)
    =>
    (send ?ins put-sub_type counselor)
    (send ?ins put-personalities intensity_developing_wholeness_private_connecting)
    (assert (idealist_flag2 yes))
    )

(defrule idealist-champion (declare (salience -1))
    ?ins <- (object (is-a TEMPERAMENT) (main_type idealist) (sub_type nil))
    (champion yes)
    (idealist_flag yes)
    =>
    (send ?ins put-sub_type champion)
    (send ?ins put-personalities passionate_networking_enthusiastic_authentic_eclectic)
    (assert (idealist_flag2 yes))
    )

(defrule idealist-healer (declare (salience -2))
    ?ins <- (object (is-a TEMPERAMENT) (main_type idealist) (sub_type nil))
    (healer yes)
    (idealist_flag yes)
    =>
    (send ?ins put-sub_type healer)
    (send ?ins put-personalities reconciling_inspiring_nurturing_fervent_dreamers)
    (assert (idealist_flag2 yes))
    )

(defrule idealist-sub_type-verify (declare (salience -3))
    ?ins <- (object (is-a TEMPERAMENT) (main_type idealist) (sub_type nil))
    (not (idealist_flag2 ?))
    =>
    (send ?ins put-sub_type healer)
    (send ?ins put-personalities reconciling_inspiring_nurturing_fervent_dreamers)
    )

;Check if the person belong to which category of rational
(defrule further-rational (declare (salience -4))
    ?ins <- (object (is-a TEMPERAMENT) (main_type rational))
    (not (rational_flag ?))
    =>
    (assert (field_marshal (yes-or-no-p "Have you ever build something from waste (yes/no)? ")))
    (assert (mastermind (yes-or-no-p "Do you strategies before palying a game (yes/no)? ")))
    (assert (inventor (yes-or-no-p "Are you curious about your surroundings (yes/no)? ")))
    (assert (architect (yes-or-no-p "Have you ever successfully described an object that cannot be seen (yes/no)? ")))
    (assert (rational_flag yes))
    )

(defrule rational-field_marshal (declare (salience -5))
    ?ins <- (object (is-a TEMPERAMENT) (main_type rational) (sub_type nil))
    (field_marshal yes)
    (rational_flag yes)
    =>
    (send ?ins put-sub_type field_marshal)
    (send ?ins put-personalities commanding_strategic_communicating_efficient_visionary)
    (assert (rational_flag2 yes))
    )

(defrule rational-mastermind (declare (salience -6))
    ?ins <- (object (is-a TEMPERAMENT) (main_type rational) (sub_type nil))
    (mastermind yes)
    (rational_flag yes)
    =>
    (send ?ins put-sub_type mastermind)
    (send ?ins put-personalities planning_selfconfident_systemic_utilitarian_ingenious)
    (assert (rational_flag2 yes))
    )

(defrule rational-inventor (declare (salience -7))
    ?ins <- (object (is-a TEMPERAMENT) (main_type rational) (sub_type nil))
    (inventor yes)
    (rational_flag yes)
    =>
    (send ?ins put-sub_type inventor)
    (send ?ins put-personalities imaginative_prospecting_prototyping_pragmatic_openminded)
    (assert (rational_flag2 yes))
    )

(defrule rational-architect (declare (salience -8))
    ?ins <- (object (is-a TEMPERAMENT) (main_type rational) (sub_type nil))
    (architect yes)
    (rational_flag yes)
    =>
    (send ?ins put-sub_type architect)
    (send ?ins put-personalities autonomy_inquiring_logical_preoccupied_complex)
    (assert (rational_flag2 yes))
    )

(defrule rational-sub_type-verify (declare (salience -9))
    ?ins <- (object (is-a TEMPERAMENT) (main_type rational) (sub_type nil))
    (not (rational_flag2 ?))
    =>
    (send ?ins put-sub_type architect)
    (send ?ins put-personalities autonomy_inquiring_logical_preoccupied_complex)
    )

;*************************************
;Final Conclusion (Giving Suggestions)
;*************************************
;Printing the Main Type, Sub Type, Personalities Traits
(defrule print-result (declare (salience -10))
    (object (is-a TEMPERAMENT) (main_type ?mt) (sub_type ?st) (personalities ?p))
    =>
    (printout t crlf)
    (printout t "You belong to " ?mt " - " ?st " personality." crlf)
    (printout t "You will be able to master these traits easily : " crlf)
    (printout t ?p crlf)
    )

;Printing suggestion to fit the CLIPS Screen
(defrule suggestion-architect (declare (salience -11))
    ?ins <- (object (is-a TEMPERAMENT) (sub_type architect))
    =>
    (printout t "Suggestions : Architects need'nt be thought of as only interested in drawing " crlf)
    (printout t "   blueprints for buildings / roads / bridges. They are the master designers " crlf) 
    (printout t "   of all kinds of theoretical systems, including school curricula, corporate " crlf) 
    (printout t "   strategies, & new technologies. For Architects, the world exists primarily " crlf) 
    (printout t "   to be analyzed, understood, explained - & re-designed. External reality in " crlf)
    (printout t "   itself is unimportant, little more than raw material to be organized into structural models." crlf)
    (printout t crlf)
    )

(defrule suggestion-inventor (declare (salience -12))
    ?ins <- (object (is-a TEMPERAMENT) (sub_type inventor))
    =>
    (printout t "Suggestions : Inventors begin building gadgets & mechanisms as young children," crlf)
    (printout t "   & never really stop, though as adults they will turn their inventiveness to " crlf) 
    (printout t "   many kinds of organizations, social as well as mechanical. There aren't " crlf) 
    (printout t "   many Inventors, say about two percent of the population, but they have " crlf) 
    (printout t "   great impact on our everyday lives. With their innovative, entrepreneurial " crlf)
    (printout t "   spirit, Inventors are always on the lookout for a better way, always eyeing " crlf)
    (printout t "   new projects, new enterprises, new processes." crlf)
    (printout t crlf)
    )

(defrule suggestion-mastermind (declare (salience -13))
    ?ins <- (object (is-a TEMPERAMENT) (sub_type mastermind))
    =>
    (printout t "Suggestions : Masterminds are head & shoulders above all the rest in " crlf)
    (printout t "   contingency planning. Complex operations involve many steps / stages, " crlf) 
    (printout t "   one following another in a necessary progression, & Masterminds are " crlf) 
    (printout t "   naturally able to grasp how each one leads to the next, & to prepare " crlf) 
    (printout t "   alternatives for difficulties that are likely to arise any step of the way. " crlf)
    (printout t "   Trying to anticipate every contingency, Masterminds never set off on their " crlf)
    (printout t "   current project without a Plan A firmly in mind, but they are always prepared to switch to Plan B / C / D if need be." crlf)
    (printout t crlf)
    )

(defrule suggestion-field_marshal (declare (salience -14))
    ?ins <- (object (is-a TEMPERAMENT) (sub_type field_marshal))
    =>
    (printout t "Suggestions : Fieldmarshals will usually rise to positions of responsibility & " crlf)
    (printout t "   enjoy being executives. They are tireless in their devotion to their jobs & " crlf) 
    (printout t "   can easily block out other areas of life for the sake of their work. Superb " crlf) 
    (printout t "   administrators in any field - medicine, law, business, education, " crlf) 
    (printout t "   government, the military - Fieldmarshals organize their units into smooth-" crlf)
    (printout t "   functioning systems, planning in advance, keeping both short-term &  " crlf)
    (printout t "   long-range objectives well in mind." crlf)
    (printout t crlf)
    )

(defrule suggestion-healer (declare (salience -15))
    ?ins <- (object (is-a TEMPERAMENT) (sub_type healer))
    =>
    (printout t "Suggestions : Healers present a calm & serene face to the world, & can seem " crlf)
    (printout t "   shy, even distant around others. But inside they're anything but serene, " crlf) 
    (printout t "   having a capacity for personal caring rarely found in the other types.  " crlf) 
    (printout t "   Healers care deeply about the inner life of a few special persons, about " crlf) 
    (printout t "   a favorite cause in the world at large. And their great passion is to heal the " crlf)
    (printout t "   conflicts that trouble individuals, that divide groups, & thus to bring " crlf)
    (printout t "   wholeness, health, to themselves, their loved ones, & their community." crlf)
    (printout t crlf)
    )

(defrule suggestion-champion (declare (salience -16))
    ?ins <- (object (is-a TEMPERAMENT) (sub_type champion))
    =>
    (printout t "Suggestions : Champions are rather rare, say three / four percent of the " crlf)
    (printout t "   population, but even more than the others they consider intense" crlf) 
    (printout t "   emotional experiences as being vital to a full life. Champions have a wide " crlf) 
    (printout t "   range & variety of emotions, & a great passion for novelty. They see " crlf) 
    (printout t "   life as an exciting drama, pregnant with possibilities for both good & evil, " crlf)
    (printout t "   & they want to experience all the meaningful events & fascinating people in the world." crlf)        
    (printout t crlf)
    )

(defrule suggestion-counselor (declare (salience -17))
    ?ins <- (object (is-a TEMPERAMENT) (sub_type counselor))
    =>
    (printout t "Suggestions : Counselors have an exceptionally strong desire to contribute to the " crlf)
    (printout t "   welfare of others, & find great personal fulfillment interacting with " crlf) 
    (printout t "   people, nurturing their personal development, guiding them to realize " crlf) 
    (printout t "   their human potential. Although they are happy working at jobs (such as " crlf) 
    (printout t "   writing) that require solitude & close attention, Counselors do quite well " crlf)
    (printout t "   with individuals / groups of people, provided that the personal " crlf)
    (printout t "   interactions are'nt superficial, & that they find some quiet, private time " crlf)
    (printout t "   every now & then to recharge their batteries." crlf)
    (printout t crlf)
    )

(defrule suggestion-teacher (declare (salience -18))
    ?ins <- (object (is-a TEMPERAMENT) (sub_type teacher))
    =>
    (printout t "Suggestions : Teachers have a natural talent for leading students & trainees " crlf)
    (printout t "   toward learning, as Idealists like to think of it, they are capable of calling " crlf) 
    (printout t "   forth each learner's potentials. But perhaps their greatest strength lies in " crlf) 
    (printout t "   their belief in their students. Teachers look for the best in their students, " crlf) 
    (printout t "   & communicate clearly that each one has untold potential, & this " crlf)
    (printout t "   confidence can inspire their students to grow & develop more than they ever thought possible." crlf)        
    (printout t crlf)
    )

(defrule suggestion-protector (declare (salience -19))
    ?ins <- (object (is-a TEMPERAMENT) (sub_type protector))
    =>
    (printout t "Suggestions : Protectors make up as much as ten percent the population, because " crlf)
    (printout t "   their primary interest is in the safety & security of those they care about " crlf) 
    (printout t "   - their family, their circle of friends, their students, their patients, their " crlf) 
    (printout t "   boss, their fellow-workers, their employees. Protectors have an " crlf) 
    (printout t "   extraordinary sense of loyalty & responsibility in their makeup, & " crlf)
    (printout t "   seem fulfilled in the degree they can shield others from the dirt & dangers of the world." crlf)
    (printout t crlf)
    )

(defrule suggestion-provider (declare (salience -20))
    ?ins <- (object (is-a TEMPERAMENT) (sub_type provider))
    =>
    (printout t "Suggestions : Providers take it upon themselves to insure the health & welfare of " crlf)
    (printout t "   those in their care, but they are also the most sociable of all the Guardians, " crlf) 
    (printout t "   & thus are the great nurturers of social institutions such as schools, " crlf) 
    (printout t "   churches, social clubs, & civic groups. Providers are very likely more than " crlf) 
    (printout t "   ten percent of the population, & this is fortunate for the rest of us, because friendly social " crlf)
    (printout t "   service is a key to their nature. Wherever they go, Providers happily give their time" crlf)
    (printout t "   & energy to make sure that the needs of others are met, & that social functions are a success." crlf)
    (printout t crlf)
    )

(defrule suggestion-inspector (declare (salience -21))
    ?ins <- (object (is-a TEMPERAMENT) (sub_type inspector))
    =>
    (printout t "Suggestions : Inspectors are extraordinarily persevering & dutiful, particularly " crlf)
    (printout t "   when it comes to keeping an eye on the people & products they are " crlf) 
    (printout t "   responsible for. In their quiet way, Inspectors see to it that rules are " crlf) 
    (printout t "   followed, laws are respected, & standards are upheld. Inspectors (as " crlf) 
    (printout t "   much as ten percent of the general population) are the true guardians of " crlf)
    (printout t "   institutions. They are patient with their work & with the procedures " crlf)
    (printout t "   within an institution, although & always with the unauthorized behavior of some people in that institution." crlf)
        
    )

(defrule suggestion-supervisor (declare (salience -22))
    ?ins <- (object (is-a TEMPERAMENT) (sub_type supervisor))
    =>
    (printout t "Suggestions : Supervisors are highly social & community-minded, with many " crlf)
    (printout t "   rising to positions of responsibility in their school, church, industry, civic " crlf) 
    (printout t "   groups. Supervisors are generous with their time & energy, & very " crlf) 
    (printout t "   often belong to a variety of service clubs, lodges, & associations, " crlf) 
    (printout t "   supporting them through steady attendance, but also taking an outspoken " crlf)
    (printout t "   leadership role. Supervisors like to take charge of groups & are comfortable issuing orders." crlf)
    (printout t crlf)
    )

(defrule suggestion-composer (declare (salience -23))
    ?ins <- (object (is-a TEMPERAMENT) (sub_type composer))
    =>
    (printout t "Suggestions : Composers are in tune with their senses, & so have a sure grasp " crlf)
    (printout t "   of what belongs, & what doesn't belong, in all kinds of works of art. " crlf) 
    (printout t "   While the other Artisans are skilled with people, tools, & entertainment, " crlf) 
    (printout t "   Composers have an exceptional ability to work with subtle differences in " crlf) 
    (printout t "   color, tone, texture, aroma, & flavor. Although Composers often put " crlf)
    (printout t "   long, lonely hours into their artistry, they are just as impulsive as the other Artisans." crlf)
    (printout t crlf)
    )

(defrule suggestion-performer (declare (salience -24))
    ?ins <- (object (is-a TEMPERAMENT) (sub_type performer))
    =>
    (printout t "Suggestions : Performers have the special ability, even among the Artisans, to " crlf)
    (printout t "   delight those around them with their warmth, their good humor, & with " crlf) 
    (printout t "   their often extraordinary skills in music, comedy, & drama. Whether on " crlf) 
    (printout t "   the job, with friends, with their families, Performers are exciting & full " crlf) 
    (printout t "   of fun, & their great social interest lies in stimulating those around them " crlf)
    (printout t "   to take a break from work & worry, to lighten up & enjoy life." crlf)
    (printout t crlf)
    )

(defrule suggestion-crafter (declare (salience -25))
    ?ins <- (object (is-a TEMPERAMENT) (sub_type crafter))
    =>
    (printout t "Suggestions : Crafters are most clearly seen in their masterful operation of tools," crlf)
    (printout t "   equipment, machines, & instruments of all kinds. Most of us use tools in " crlf) 
    (printout t "   some capacity, of course, but Crafters (as much as ten percent of the " crlf) 
    (printout t "   population) are the true masters of tool work, with an innate ability to " crlf) 
    (printout t "   command tools & to become expert at all the crafts requiring tool skills. " crlf)
    (printout t "   Even from an early age they are drawn to tools as if to a magnet -- tools fall " crlf)
    (printout t "   into their hands demanding use, & they must work with them. Crafters are hard to get to know." crlf)
    (printout t crlf)
    )

(defrule suggestion-promoter (declare (salience -26))
    ?ins <- (object (is-a TEMPERAMENT) (sub_type promoter))
    =>
    (printout t "Suggestions : Promoters live with a theatrical flourish" crlf)
    (printout t "   which makes even the most routine events seem exciting. Not that they" crlf) 
    (printout t "   waste much time on routine events. In work & in play, Promoters demand" crlf) 
    (printout t "   new activities & new challenges. Bold & daring at heart, & ever-optimistic" crlf) 
    (printout t "   that things will go their way, Promoters will take tremendous risks to" crlf)
    (printout t "   get what they want, & seem exhilarated by walking close to the edge of disaster." crlf)
    (printout t crlf)
    )