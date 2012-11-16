

(board 'gen
       (static 'inventory-frame (vec 0 0))
       (ent 'inventory-button))
(on (changed 'inventory)
    (g/do (for ([item (g/get 'inventory)])
            ))
    )
    



(board 'tableau-du-poussin
       (background 'ms-bg-1)
       (static 'poulet-jaune-1 (vec 44 0))
       (ent 'poussin-1))

(at 'begin
 (play-anim 'poussin-1 'poussin-immobile)
 (move 'poussin-1 (vec 10 10))
 (add-effect 'poussin-1 'glow-in-the-dark))

(at (time (sec 10))
    (seq (par (play-anim 'poussin-1 'poussin-qui-dance)
              (goto 'poussin-1 (vec 40 40)))
         (goto 'poussin-1 (vec 20 20))
         (goto 'poussin-1 (vec 40 20)))
    (remove-effect 'poussin-1 'glow-in-the-dark)
    (do-when (is-flag-set 'poussin-mort)
             (play-anim 'poussin-1 'poussin-mort))
    (play-sound 'blop))
         
(on (clicked-on 'poussin-1)
    (set-flag 'poussin-mort)
    (g/do (append! (g/get 'a) 'poussin-mort)
    (hide 'poussin-1))
