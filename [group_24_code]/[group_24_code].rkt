;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname game8) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;-------------------------structures--------------------------
; struct: pos
; property: x(number): x-coordinate of the position
; property: y(number): y-coordinate of the position
(define-struct pos (x y))

; struct: obj
; property: im(image): image of the object
; property: pos(pos): position of the object 
(define-struct obj (im pos))

; struct: shapes
; property: circle square triangle(obj): moving objects in the game
; property: mainCircle mainSquare mainTriangle(obj): immovable objects in the game
; property: score(number): score of the game. If moving objects combine with the immovable objects, the score will increase.
; property: button(button): button to switch from shapes scene to numbers scene
(define-struct shapes (circle mainCircle mainSquare mainTriangle square triangle score button))

; struct: numbers
; property: n0 n1 ... n9(obj): moving objects in the game. pictures of numbers in animal shape.
; property: star0 star1 ... star9(obj): immovable objects in the game. circles with increasing numbers of stars inside them.
; property: score(number): score of the game. if the correct numbers match the correct number of stars the score will increase.
; property: button2(button): button to switch from numbers scene to colors scene
(define-struct numbers (n0 n1 n2 n3 n4 n5 n6 n7 n8 n9 star0 star1 star2 star3 star4 star5 star6 star7 star8 star9 score button2))

; struct: colors
; property: f1 f2 ... f8(obj): moving objects in the game. pictures of fruits with different colors.
; property: c1 c2 ... c8(obj): immovable objects in the game. Square objects containing 8 different colors.
; property: score(number): score of the game. If the correct colored fruit matches the correct colored square the score will increase.
; property: button3(button): button to switch from colors scene to numbers scene
(define-struct colors (f1 f2 f3 f4 f5 f6 f7 f8 c1 c2 c3 c4 c5 c6 c7 c8 score button3))

; struct: menu
; property: startGame(button): button for starting the game
(define-struct menu (startGame background))

; struct: gameOver
; property: over(string): game over text
; property: balloon1(obj): balloon object
; property: balloon2(obj): balloon object
(define-struct gameOver (over balloon1 balloon2))

; struct: game
; property: shapes(shapes): shapes scene, first part of the game
; property: numbers(numbers): numbers scene, second part of the game
; property: colors(colors): colors scene, third part of the game
; property: score(number): score of the game. If the objects combine with the correct objects, the score will increase.
; property: menu(menu): menu scene
; property: gameOver(gameOver): game over scene
; property: current_scene(string): current scene of the game
(define-struct game (shapes numbers colors score menu gameOver current_scene))

;-------------------------useful functions--------------------------
; purpose: calculate the distance between given position and x, y values for cursor position
; contract: calcDist: p(pos) x(number) y(number) -> number
; test
(check-expect (calcDist (make-pos 0 0) 6 8) 10)
; function
(define (calcDist p x y) (sqrt (+ (sqr (- (pos-x p) x))
                                  (sqr (- (pos-y p) y)))))

; purpose: Determines whether a point (x, y) is inside the object 'o'. The object 'o' is defined by its position and visual properties.
; contract: isInObject: o(obj) x(number) y(number) -> boolean
; test
(check-expect (isInObject (make-obj (circle 50 "solid" "blue") (make-pos 200 200)) 175 175) #true)
; functio
(define (isInObject o x y) (<= (calcDist (obj-pos o) x y)
                               (/ (image-width (obj-im o)) 2)
                               ))

; purpose: Check whether moving objects match their counterparts, immovable objects, or not.
; contract: isCollided: c(obj) s(obj) offset(number) -> boolean
; test
(check-expect (isCollided (make-obj (circle 50 "solid" "red") (make-pos 100 100))
                          (make-obj (circle 50 "solid" "red") (make-pos 400 250)) 0) #false)
(check-expect (isCollided (make-obj (circle 50 "solid" "red") (make-pos 400 250))
                          (make-obj (circle 50 "solid" "red") (make-pos 400 250)) 0) #true)
; function
(define (isCollided c s offset)(<= (calcDist (obj-pos c)
                                             (pos-x (obj-pos s))
                                             (pos-y (obj-pos s))) offset))

;-------------------------update functions--------------------------
; purpose: updates the score value with the given amount
; contract: updateGameScore: s(number) amount(number) -> number
; test:
(check-expect (updateGameScore 0 50) 50)
; function
(define (updateGameScore s amount) (+ s amount))

; purpose: update the scene based on certain conditions
; contract: updateScene: g(game) new_scene(string) -> game
; function:
(define (updateScene g new_scene)
  (make-game (game-shapes g)
             (game-numbers g)
             (game-colors g)
             (game-score g)
             (game-menu g)
             (game-gameOver g)
             new_scene))


; purpose: update the game with respect to the mouse interaction. If the moving objects combine with the correct objects, the score will increase.
; contract: updateGameOnMouse: g(game) x(number) y(number) event(mouseEvent)-> game
; function
(define (updateGameOnMouse g x y event)
  (cond
    ((and (string=? (game-current_scene g) "menu")
          (mouse=? event "button-down") ;To open the shapes scene when startGame button is clicked
          (isInObject (menu-startGame (game-menu g)) x y))
     (make-game (game-shapes g)
                (game-numbers g)
                (game-colors g)
                (game-score g)
                (game-menu g)
                (game-gameOver g)
                "shapes"))

    ((and (string=? (game-current_scene g) "shapes")
          (mouse=? event "button-down") ;To open the numbers scene when button is clicked
          (>= (shapes-score (game-shapes g)) 150)
          (isInObject (shapes-button (game-shapes g)) x y))
     (make-game (game-shapes g)
                (game-numbers g)
                (game-colors g)
                (game-score g)
                (game-menu g)
                (game-gameOver g)
                "numbers"))
    
    ((and (string=? (game-current_scene g) "numbers")
          (mouse=? event "button-down") ;To open the colors scene when button2 is clicked
          (>= (numbers-score (game-numbers g)) 650)
          (isInObject (numbers-button2 (game-numbers g)) x y))
     (make-game (game-shapes g)
                (game-numbers g)
                (game-colors g)
                (game-score g)
                (game-menu g)
                (game-gameOver g)
                "colors"))

    ((and (string=? (game-current_scene g) "colors")
          (mouse=? event "button-down") ;To open the gameOver scene when button3 is clicked
          (>= (colors-score (game-colors g)) 1050)
          (isInObject (colors-button3 (game-colors g)) x y))
     (make-game (game-shapes g)
                (game-numbers g)
                (game-colors g)
                (game-score g)
                (game-menu g)
                (game-gameOver g)
                "gameOver"))
    
    ;for the shapes part
    ((isCollided (shapes-circle (game-shapes g))(shapes-mainCircle (game-shapes g)) 37)
      (make-game (make-shapes
      (make-obj (obj-im (shapes-circle (game-shapes g)))(make-pos -200 -200))
      (shapes-mainCircle (game-shapes g))
      (shapes-mainSquare (game-shapes g))
      (shapes-mainTriangle (game-shapes g))
      (shapes-square (game-shapes g))
      (shapes-triangle (game-shapes g))
      (updateGameScore (game-score g) 50)
      (shapes-button (game-shapes g)))
      (game-numbers g) (game-colors g) (updateGameScore (game-score g) 50) (game-menu g) (game-gameOver g) "shapes"))

    ((isCollided (shapes-square (game-shapes g))(shapes-mainSquare (game-shapes g)) 37)
      (make-game (make-shapes
      (shapes-circle (game-shapes g))
      (shapes-mainCircle (game-shapes g))
      (shapes-mainSquare (game-shapes g))
      (shapes-mainTriangle (game-shapes g))
      (make-obj (obj-im (shapes-square (game-shapes g)))(make-pos -200 -200))
      (shapes-triangle (game-shapes g))
      (updateGameScore (game-score g) 50)
      (shapes-button (game-shapes g)))
      (game-numbers g) (game-colors g) (updateGameScore (game-score g) 50) (game-menu g) (game-gameOver g) "shapes"))

    ((isCollided (shapes-triangle (game-shapes g))(shapes-mainTriangle (game-shapes g)) 37)
      (make-game (make-shapes
      (shapes-circle (game-shapes g))
      (shapes-mainCircle (game-shapes g))
      (shapes-mainSquare (game-shapes g))
      (shapes-mainTriangle (game-shapes g))
      (shapes-square (game-shapes g))
      (make-obj (obj-im (shapes-triangle (game-shapes g)))(make-pos -200 -200))
      (updateGameScore (game-score g) 50)
      (shapes-button (game-shapes g))) 
      (game-numbers g) (game-colors g) (updateGameScore (game-score g) 50) (game-menu g) (game-gameOver g) "shapes"))

    ((and (mouse=? event "drag") (isInObject (shapes-circle (game-shapes g)) x y))
         (make-game (make-shapes
          (make-obj (obj-im (shapes-circle (game-shapes g))) (make-pos x y))
          (shapes-mainCircle (game-shapes g))
          (shapes-mainSquare (game-shapes g))
          (shapes-mainTriangle (game-shapes g))
          (shapes-square (game-shapes g))
          (shapes-triangle (game-shapes g))
          (game-score g)
          (shapes-button (game-shapes g)))
                    (game-numbers g)
                    (game-colors g)
                    (game-score g)
                    (game-menu g)
                    (game-gameOver g)
                    (game-current_scene g)))
    
    ((and (mouse=? event "drag") (isInObject (shapes-square (game-shapes g)) x y))
         (make-game (make-shapes
          (shapes-circle (game-shapes g))
          (shapes-mainCircle (game-shapes g))
          (shapes-mainSquare (game-shapes g))
          (shapes-mainTriangle (game-shapes g))
          (make-obj (obj-im (shapes-square (game-shapes g))) (make-pos x y))
          (shapes-triangle (game-shapes g))
          (game-score g)
          (shapes-button (game-shapes g)))
                    (game-numbers g)
                    (game-colors g)
                    (game-score g)
                    (game-menu g)
                    (game-gameOver g)
                    (game-current_scene g)))

    ((and (mouse=? event "drag") (isInObject (shapes-triangle (game-shapes g)) x y))
         (make-game (make-shapes
          (shapes-circle (game-shapes g))
          (shapes-mainCircle (game-shapes g))
          (shapes-mainSquare (game-shapes g))
          (shapes-mainTriangle (game-shapes g))
          (shapes-square (game-shapes g))
          (make-obj (obj-im (shapes-triangle (game-shapes g))) (make-pos x y))
          (game-score g)
          (shapes-button (game-shapes g)))
                    (game-numbers g)
                    (game-colors g)
                    (game-score g)
                    (game-menu g)
                    (game-gameOver g)
                    (game-current_scene g)))
    
     ;for the numbers part
     ((isCollided (numbers-n0 (game-numbers g))(numbers-star0 (game-numbers g)) 35) (make-game (game-shapes g) (make-numbers
      (make-obj (obj-im (numbers-n0 (game-numbers g)))(make-pos -200 -200)) (numbers-n1 (game-numbers g)) (numbers-n2 (game-numbers g)) (numbers-n3 (game-numbers g)) (numbers-n4 (game-numbers g))
      (numbers-n5 (game-numbers g)) (numbers-n6 (game-numbers g)) (numbers-n7 (game-numbers g)) (numbers-n8 (game-numbers g)) (numbers-n9 (game-numbers g))
      (numbers-star0 (game-numbers g)) (numbers-star1 (game-numbers g)) (numbers-star2 (game-numbers g)) (numbers-star3 (game-numbers g)) (numbers-star4 (game-numbers g))
      (numbers-star5 (game-numbers g)) (numbers-star6 (game-numbers g)) (numbers-star7 (game-numbers g)) (numbers-star8 (game-numbers g)) (numbers-star9 (game-numbers g))
      (updateGameScore (game-score g) 50) (numbers-button2 (game-numbers g))
      ) (game-colors g) (updateGameScore (game-score g) 50) (game-menu g) (game-gameOver g) "numbers"))
      ((isCollided (numbers-n1 (game-numbers g))(numbers-star1 (game-numbers g)) 35) (make-game (game-shapes g) (make-numbers
      (numbers-n0 (game-numbers g)) (make-obj (obj-im (numbers-n1 (game-numbers g)))(make-pos -200 -200)) (numbers-n2 (game-numbers g)) (numbers-n3 (game-numbers g)) (numbers-n4 (game-numbers g))
      (numbers-n5 (game-numbers g)) (numbers-n6 (game-numbers g)) (numbers-n7 (game-numbers g)) (numbers-n8 (game-numbers g)) (numbers-n9 (game-numbers g))
      (numbers-star0 (game-numbers g)) (numbers-star1 (game-numbers g)) (numbers-star2 (game-numbers g)) (numbers-star3 (game-numbers g)) (numbers-star4 (game-numbers g))
      (numbers-star5 (game-numbers g)) (numbers-star6 (game-numbers g)) (numbers-star7 (game-numbers g)) (numbers-star8 (game-numbers g)) (numbers-star9 (game-numbers g))
      (updateGameScore (game-score g) 50) (numbers-button2 (game-numbers g))
      ) (game-colors g) (updateGameScore (game-score g) 50) (game-menu g) (game-gameOver g) "numbers"))
      ((isCollided (numbers-n2 (game-numbers g))(numbers-star2 (game-numbers g)) 35) (make-game (game-shapes g) (make-numbers
      (numbers-n0 (game-numbers g)) (numbers-n1 (game-numbers g)) (make-obj (obj-im (numbers-n2 (game-numbers g)))(make-pos -200 -200)) (numbers-n3 (game-numbers g)) (numbers-n4 (game-numbers g))
      (numbers-n5 (game-numbers g)) (numbers-n6 (game-numbers g)) (numbers-n7 (game-numbers g)) (numbers-n8 (game-numbers g)) (numbers-n9 (game-numbers g))
      (numbers-star0 (game-numbers g)) (numbers-star1 (game-numbers g)) (numbers-star2 (game-numbers g)) (numbers-star3 (game-numbers g)) (numbers-star4 (game-numbers g))
      (numbers-star5 (game-numbers g)) (numbers-star6 (game-numbers g)) (numbers-star7 (game-numbers g)) (numbers-star8 (game-numbers g)) (numbers-star9 (game-numbers g))
      (updateGameScore (game-score g) 50) (numbers-button2 (game-numbers g))
      ) (game-colors g) (updateGameScore (game-score g) 50) (game-menu g) (game-gameOver g) "numbers"))
      ((isCollided (numbers-n3 (game-numbers g))(numbers-star3 (game-numbers g)) 35) (make-game (game-shapes g) (make-numbers
      (numbers-n0 (game-numbers g)) (numbers-n1 (game-numbers g)) (numbers-n2 (game-numbers g)) (make-obj (obj-im (numbers-n3 (game-numbers g)))(make-pos -200 -200)) (numbers-n4 (game-numbers g))
      (numbers-n5 (game-numbers g)) (numbers-n6 (game-numbers g)) (numbers-n7 (game-numbers g)) (numbers-n8 (game-numbers g)) (numbers-n9 (game-numbers g))
      (numbers-star0 (game-numbers g)) (numbers-star1 (game-numbers g)) (numbers-star2 (game-numbers g)) (numbers-star3 (game-numbers g)) (numbers-star4 (game-numbers g))
      (numbers-star5 (game-numbers g)) (numbers-star6 (game-numbers g)) (numbers-star7 (game-numbers g)) (numbers-star8 (game-numbers g)) (numbers-star9 (game-numbers g))
      (updateGameScore (game-score g) 50) (numbers-button2 (game-numbers g))
      ) (game-colors g) (updateGameScore (game-score g) 50) (game-menu g) (game-gameOver g) "numbers"))
      ((isCollided (numbers-n4 (game-numbers g))(numbers-star4 (game-numbers g)) 35) (make-game (game-shapes g) (make-numbers
      (numbers-n0 (game-numbers g)) (numbers-n1 (game-numbers g)) (numbers-n2 (game-numbers g)) (numbers-n3 (game-numbers g)) (make-obj (obj-im (numbers-n4 (game-numbers g)))(make-pos -200 -200))
      (numbers-n5 (game-numbers g)) (numbers-n6 (game-numbers g)) (numbers-n7 (game-numbers g)) (numbers-n8 (game-numbers g)) (numbers-n9 (game-numbers g))
      (numbers-star0 (game-numbers g)) (numbers-star1 (game-numbers g)) (numbers-star2 (game-numbers g)) (numbers-star3 (game-numbers g)) (numbers-star4 (game-numbers g))
      (numbers-star5 (game-numbers g)) (numbers-star6 (game-numbers g)) (numbers-star7 (game-numbers g)) (numbers-star8 (game-numbers g)) (numbers-star9 (game-numbers g))
      (updateGameScore (game-score g) 50) (numbers-button2 (game-numbers g))
      ) (game-colors g) (updateGameScore (game-score g) 50) (game-menu g) (game-gameOver g) "numbers"))
      ((isCollided (numbers-n5 (game-numbers g))(numbers-star5 (game-numbers g)) 35) (make-game (game-shapes g) (make-numbers
      (numbers-n0 (game-numbers g)) (numbers-n1 (game-numbers g)) (numbers-n2 (game-numbers g)) (numbers-n3 (game-numbers g)) (numbers-n4 (game-numbers g))
      (make-obj (obj-im (numbers-n5 (game-numbers g)))(make-pos -200 -200)) (numbers-n6 (game-numbers g)) (numbers-n7 (game-numbers g)) (numbers-n8 (game-numbers g)) (numbers-n9 (game-numbers g))
      (numbers-star0 (game-numbers g)) (numbers-star1 (game-numbers g)) (numbers-star2 (game-numbers g)) (numbers-star3 (game-numbers g)) (numbers-star4 (game-numbers g))
      (numbers-star5 (game-numbers g)) (numbers-star6 (game-numbers g)) (numbers-star7 (game-numbers g)) (numbers-star8 (game-numbers g)) (numbers-star9 (game-numbers g))
      (updateGameScore (game-score g) 50) (numbers-button2 (game-numbers g))
      ) (game-colors g) (updateGameScore (game-score g) 50) (game-menu g) (game-gameOver g) "numbers"))
      ((isCollided (numbers-n6 (game-numbers g))(numbers-star6 (game-numbers g)) 35) (make-game (game-shapes g) (make-numbers
      (numbers-n0 (game-numbers g)) (numbers-n1 (game-numbers g)) (numbers-n2 (game-numbers g)) (numbers-n3 (game-numbers g)) (numbers-n4 (game-numbers g))
      (numbers-n5 (game-numbers g)) (make-obj (obj-im (numbers-n6 (game-numbers g)))(make-pos -200 -200)) (numbers-n7 (game-numbers g)) (numbers-n8 (game-numbers g)) (numbers-n9 (game-numbers g))
      (numbers-star0 (game-numbers g)) (numbers-star1 (game-numbers g)) (numbers-star2 (game-numbers g)) (numbers-star3 (game-numbers g)) (numbers-star4 (game-numbers g))
      (numbers-star5 (game-numbers g)) (numbers-star6 (game-numbers g)) (numbers-star7 (game-numbers g)) (numbers-star8 (game-numbers g)) (numbers-star9 (game-numbers g))
      (updateGameScore (game-score g) 50) (numbers-button2 (game-numbers g))
      ) (game-colors g) (updateGameScore (game-score g) 50) (game-menu g) (game-gameOver g) "numbers"))
      ((isCollided (numbers-n7 (game-numbers g))(numbers-star7 (game-numbers g)) 35) (make-game (game-shapes g) (make-numbers
      (numbers-n0 (game-numbers g)) (numbers-n1 (game-numbers g)) (numbers-n2 (game-numbers g)) (numbers-n3 (game-numbers g)) (numbers-n4 (game-numbers g))
      (numbers-n5 (game-numbers g)) (numbers-n6 (game-numbers g)) (make-obj (obj-im (numbers-n7 (game-numbers g)))(make-pos -200 -200)) (numbers-n8 (game-numbers g)) (numbers-n9 (game-numbers g))
      (numbers-star0 (game-numbers g)) (numbers-star1 (game-numbers g)) (numbers-star2 (game-numbers g)) (numbers-star3 (game-numbers g)) (numbers-star4 (game-numbers g))
      (numbers-star5 (game-numbers g)) (numbers-star6 (game-numbers g)) (numbers-star7 (game-numbers g)) (numbers-star8 (game-numbers g)) (numbers-star9 (game-numbers g))
      (updateGameScore (game-score g) 50) (numbers-button2 (game-numbers g))
      ) (game-colors g) (updateGameScore (game-score g) 50) (game-menu g) (game-gameOver g) "numbers"))
      ((isCollided (numbers-n8 (game-numbers g))(numbers-star8 (game-numbers g)) 35) (make-game (game-shapes g) (make-numbers
      (numbers-n0 (game-numbers g)) (numbers-n1 (game-numbers g)) (numbers-n2 (game-numbers g)) (numbers-n3 (game-numbers g)) (numbers-n4 (game-numbers g))
      (numbers-n5 (game-numbers g)) (numbers-n6 (game-numbers g)) (numbers-n7 (game-numbers g)) (make-obj (obj-im (numbers-n8 (game-numbers g)))(make-pos -200 -200)) (numbers-n9 (game-numbers g))
      (numbers-star0 (game-numbers g)) (numbers-star1 (game-numbers g)) (numbers-star2 (game-numbers g)) (numbers-star3 (game-numbers g)) (numbers-star4 (game-numbers g))
      (numbers-star5 (game-numbers g)) (numbers-star6 (game-numbers g)) (numbers-star7 (game-numbers g)) (numbers-star8 (game-numbers g)) (numbers-star9 (game-numbers g))
      (updateGameScore (game-score g) 50) (numbers-button2 (game-numbers g))
      ) (game-colors g) (updateGameScore (game-score g) 50) (game-menu g) (game-gameOver g) "numbers"))
      ((isCollided (numbers-n9 (game-numbers g))(numbers-star9 (game-numbers g)) 35) (make-game (game-shapes g) (make-numbers
      (numbers-n0 (game-numbers g)) (numbers-n1 (game-numbers g)) (numbers-n2 (game-numbers g)) (numbers-n3 (game-numbers g)) (numbers-n4 (game-numbers g))
      (numbers-n5 (game-numbers g)) (numbers-n6 (game-numbers g)) (numbers-n7 (game-numbers g)) (numbers-n8 (game-numbers g)) (make-obj (obj-im (numbers-n9 (game-numbers g)))(make-pos -200 -200))
      (numbers-star0 (game-numbers g)) (numbers-star1 (game-numbers g)) (numbers-star2 (game-numbers g)) (numbers-star3 (game-numbers g)) (numbers-star4 (game-numbers g))
      (numbers-star5 (game-numbers g)) (numbers-star6 (game-numbers g)) (numbers-star7 (game-numbers g)) (numbers-star8 (game-numbers g)) (numbers-star9 (game-numbers g))
      (updateGameScore (game-score g) 50) (numbers-button2 (game-numbers g))
      ) (game-colors g) (updateGameScore (game-score g) 50) (game-menu g) (game-gameOver g) "numbers"))

    ((and (mouse=? event "drag") (isInObject (numbers-n0 (game-numbers g)) x y)) 
      (make-game (game-shapes g) (make-numbers
      (make-obj (obj-im (numbers-n0 (game-numbers g)))(make-pos x y)) (numbers-n1 (game-numbers g)) (numbers-n2 (game-numbers g)) (numbers-n3 (game-numbers g)) (numbers-n4 (game-numbers g))
      (numbers-n5 (game-numbers g)) (numbers-n6 (game-numbers g)) (numbers-n7 (game-numbers g)) (numbers-n8 (game-numbers g)) (numbers-n9 (game-numbers g))
      (numbers-star0 (game-numbers g)) (numbers-star1 (game-numbers g)) (numbers-star2 (game-numbers g)) (numbers-star3 (game-numbers g)) (numbers-star4 (game-numbers g))
      (numbers-star5 (game-numbers g)) (numbers-star6 (game-numbers g)) (numbers-star7 (game-numbers g)) (numbers-star8 (game-numbers g)) (numbers-star9 (game-numbers g)) 
      (game-score g) (numbers-button2 (game-numbers g)))
      (game-colors g) (game-score g) (game-menu g) (game-gameOver g) (game-current_scene g)))
    ((and (mouse=? event "drag") (isInObject (numbers-n1 (game-numbers g)) x y))
      (make-game (game-shapes g) (make-numbers
      (numbers-n0 (game-numbers g)) (make-obj (obj-im (numbers-n1 (game-numbers g)))(make-pos x y)) (numbers-n2 (game-numbers g)) (numbers-n3 (game-numbers g)) (numbers-n4 (game-numbers g))
      (numbers-n5 (game-numbers g)) (numbers-n6 (game-numbers g)) (numbers-n7 (game-numbers g)) (numbers-n8 (game-numbers g)) (numbers-n9 (game-numbers g))
      (numbers-star0 (game-numbers g)) (numbers-star1 (game-numbers g)) (numbers-star2 (game-numbers g)) (numbers-star3 (game-numbers g)) (numbers-star4 (game-numbers g))
      (numbers-star5 (game-numbers g)) (numbers-star6 (game-numbers g)) (numbers-star7 (game-numbers g)) (numbers-star8 (game-numbers g)) (numbers-star9 (game-numbers g)) 
      (game-score g) (numbers-button2 (game-numbers g)))
      (game-colors g) (game-score g) (game-menu g) (game-gameOver g) (game-current_scene g)))
    ((and (mouse=? event "drag") (isInObject (numbers-n2 (game-numbers g)) x y))
      (make-game (game-shapes g) (make-numbers
      (numbers-n0 (game-numbers g)) (numbers-n1 (game-numbers g)) (make-obj (obj-im (numbers-n2 (game-numbers g)))(make-pos x y)) (numbers-n3 (game-numbers g)) (numbers-n4 (game-numbers g))
      (numbers-n5 (game-numbers g)) (numbers-n6 (game-numbers g)) (numbers-n7 (game-numbers g)) (numbers-n8 (game-numbers g)) (numbers-n9 (game-numbers g))
      (numbers-star0 (game-numbers g)) (numbers-star1 (game-numbers g)) (numbers-star2 (game-numbers g)) (numbers-star3 (game-numbers g)) (numbers-star4 (game-numbers g))
      (numbers-star5 (game-numbers g)) (numbers-star6 (game-numbers g)) (numbers-star7 (game-numbers g)) (numbers-star8 (game-numbers g)) (numbers-star9 (game-numbers g)) 
      (game-score g) (numbers-button2 (game-numbers g)))
      (game-colors g) (game-score g) (game-menu g) (game-gameOver g) (game-current_scene g)))
    ((and (mouse=? event "drag") (isInObject (numbers-n3 (game-numbers g)) x y))
      (make-game (game-shapes g) (make-numbers
      (numbers-n0 (game-numbers g)) (numbers-n1 (game-numbers g)) (numbers-n2 (game-numbers g)) (make-obj (obj-im (numbers-n3 (game-numbers g)))(make-pos x y)) (numbers-n4 (game-numbers g))
      (numbers-n5 (game-numbers g)) (numbers-n6 (game-numbers g)) (numbers-n7 (game-numbers g)) (numbers-n8 (game-numbers g)) (numbers-n9 (game-numbers g))
      (numbers-star0 (game-numbers g)) (numbers-star1 (game-numbers g)) (numbers-star2 (game-numbers g)) (numbers-star3 (game-numbers g)) (numbers-star4 (game-numbers g))
      (numbers-star5 (game-numbers g)) (numbers-star6 (game-numbers g)) (numbers-star7 (game-numbers g)) (numbers-star8 (game-numbers g)) (numbers-star9 (game-numbers g)) 
      (game-score g) (numbers-button2 (game-numbers g)))
      (game-colors g) (game-score g) (game-menu g) (game-gameOver g) (game-current_scene g)))
    ((and (mouse=? event "drag") (isInObject (numbers-n4 (game-numbers g)) x y))
      (make-game (game-shapes g) (make-numbers
      (numbers-n0 (game-numbers g)) (numbers-n1 (game-numbers g)) (numbers-n2 (game-numbers g)) (numbers-n3 (game-numbers g)) (make-obj (obj-im (numbers-n4 (game-numbers g)))(make-pos x y))
      (numbers-n5 (game-numbers g)) (numbers-n6 (game-numbers g)) (numbers-n7 (game-numbers g)) (numbers-n8 (game-numbers g)) (numbers-n9 (game-numbers g))
      (numbers-star0 (game-numbers g)) (numbers-star1 (game-numbers g)) (numbers-star2 (game-numbers g)) (numbers-star3 (game-numbers g)) (numbers-star4 (game-numbers g))
      (numbers-star5 (game-numbers g)) (numbers-star6 (game-numbers g)) (numbers-star7 (game-numbers g)) (numbers-star8 (game-numbers g)) (numbers-star9 (game-numbers g)) 
      (game-score g) (numbers-button2 (game-numbers g)))
      (game-colors g) (game-score g) (game-menu g) (game-gameOver g) (game-current_scene g)))
    ((and (mouse=? event "drag") (isInObject (numbers-n5 (game-numbers g)) x y))
      (make-game (game-shapes g) (make-numbers
      (numbers-n0 (game-numbers g)) (numbers-n1 (game-numbers g)) (numbers-n2 (game-numbers g)) (numbers-n3 (game-numbers g)) (numbers-n4 (game-numbers g))
      (make-obj (obj-im (numbers-n5 (game-numbers g)))(make-pos x y)) (numbers-n6 (game-numbers g)) (numbers-n7 (game-numbers g)) (numbers-n8 (game-numbers g)) (numbers-n9 (game-numbers g))
      (numbers-star0 (game-numbers g)) (numbers-star1 (game-numbers g)) (numbers-star2 (game-numbers g)) (numbers-star3 (game-numbers g)) (numbers-star4 (game-numbers g))
      (numbers-star5 (game-numbers g)) (numbers-star6 (game-numbers g)) (numbers-star7 (game-numbers g)) (numbers-star8 (game-numbers g)) (numbers-star9 (game-numbers g)) 
      (game-score g) (numbers-button2 (game-numbers g)))
      (game-colors g) (game-score g) (game-menu g) (game-gameOver g) (game-current_scene g)))
    ((and (mouse=? event "drag") (isInObject (numbers-n6 (game-numbers g)) x y))
      (make-game (game-shapes g) (make-numbers
      (numbers-n0 (game-numbers g)) (numbers-n1 (game-numbers g)) (numbers-n2 (game-numbers g)) (numbers-n3 (game-numbers g)) (numbers-n4 (game-numbers g))
      (numbers-n5 (game-numbers g)) (make-obj (obj-im (numbers-n6 (game-numbers g)))(make-pos x y)) (numbers-n7 (game-numbers g)) (numbers-n8 (game-numbers g)) (numbers-n9 (game-numbers g))
      (numbers-star0 (game-numbers g)) (numbers-star1 (game-numbers g)) (numbers-star2 (game-numbers g)) (numbers-star3 (game-numbers g)) (numbers-star4 (game-numbers g))
      (numbers-star5 (game-numbers g)) (numbers-star6 (game-numbers g)) (numbers-star7 (game-numbers g)) (numbers-star8 (game-numbers g)) (numbers-star9 (game-numbers g)) 
      (game-score g) (numbers-button2 (game-numbers g)))
      (game-colors g) (game-score g) (game-menu g) (game-gameOver g) (game-current_scene g)))
    ((and (mouse=? event "drag") (isInObject (numbers-n7 (game-numbers g)) x y))
      (make-game (game-shapes g) (make-numbers
      (numbers-n0 (game-numbers g)) (numbers-n1 (game-numbers g)) (numbers-n2 (game-numbers g)) (numbers-n3 (game-numbers g)) (numbers-n4 (game-numbers g))
      (numbers-n5 (game-numbers g)) (numbers-n6 (game-numbers g)) (make-obj (obj-im (numbers-n7 (game-numbers g)))(make-pos x y)) (numbers-n8 (game-numbers g)) (numbers-n9 (game-numbers g))
      (numbers-star0 (game-numbers g)) (numbers-star1 (game-numbers g)) (numbers-star2 (game-numbers g)) (numbers-star3 (game-numbers g)) (numbers-star4 (game-numbers g))
      (numbers-star5 (game-numbers g)) (numbers-star6 (game-numbers g)) (numbers-star7 (game-numbers g)) (numbers-star8 (game-numbers g)) (numbers-star9 (game-numbers g)) 
      (game-score g) (numbers-button2 (game-numbers g)))
      (game-colors g) (game-score g) (game-menu g) (game-gameOver g) (game-current_scene g)))
    ((and (mouse=? event "drag") (isInObject (numbers-n8 (game-numbers g)) x y))
      (make-game (game-shapes g) (make-numbers
      (numbers-n0 (game-numbers g)) (numbers-n1 (game-numbers g)) (numbers-n2 (game-numbers g)) (numbers-n3 (game-numbers g)) (numbers-n4 (game-numbers g))
      (numbers-n5 (game-numbers g)) (numbers-n6 (game-numbers g)) (numbers-n7 (game-numbers g)) (make-obj (obj-im (numbers-n8 (game-numbers g)))(make-pos x y)) (numbers-n9 (game-numbers g))
      (numbers-star0 (game-numbers g)) (numbers-star1 (game-numbers g)) (numbers-star2 (game-numbers g)) (numbers-star3 (game-numbers g)) (numbers-star4 (game-numbers g))
      (numbers-star5 (game-numbers g)) (numbers-star6 (game-numbers g)) (numbers-star7 (game-numbers g)) (numbers-star8 (game-numbers g)) (numbers-star9 (game-numbers g)) 
      (game-score g) (numbers-button2 (game-numbers g)))
      (game-colors g) (game-score g) (game-menu g) (game-gameOver g) (game-current_scene g)))
    ((and (mouse=? event "drag") (isInObject (numbers-n9 (game-numbers g)) x y))
      (make-game (game-shapes g) (make-numbers
      (numbers-n0 (game-numbers g)) (numbers-n1 (game-numbers g)) (numbers-n2 (game-numbers g)) (numbers-n3 (game-numbers g)) (numbers-n4 (game-numbers g))
      (numbers-n5 (game-numbers g)) (numbers-n6 (game-numbers g)) (numbers-n7 (game-numbers g)) (numbers-n8 (game-numbers g)) (make-obj (obj-im (numbers-n9 (game-numbers g)))(make-pos x y))
      (numbers-star0 (game-numbers g)) (numbers-star1 (game-numbers g)) (numbers-star2 (game-numbers g)) (numbers-star3 (game-numbers g)) (numbers-star4 (game-numbers g))
      (numbers-star5 (game-numbers g)) (numbers-star6 (game-numbers g)) (numbers-star7 (game-numbers g)) (numbers-star8 (game-numbers g)) (numbers-star9 (game-numbers g)) 
      (game-score g) (numbers-button2 (game-numbers g)))
      (game-colors g) (game-score g) (game-menu g) (game-gameOver g) (game-current_scene g)))

                                         ;for the colors part
                                         ((isCollided (colors-f1 (game-colors g))(colors-c1 (game-colors g)) 30) (make-game (game-shapes g) (game-numbers g) (make-colors
      (make-obj (obj-im (colors-f1 (game-colors g)))(make-pos -200 -200)) (colors-f2 (game-colors g)) (colors-f3 (game-colors g)) (colors-f4 (game-colors g))
      (colors-f5 (game-colors g)) (colors-f6 (game-colors g)) (colors-f7 (game-colors g)) (colors-f8 (game-colors g))
      (colors-c1 (game-colors g)) (colors-c2 (game-colors g)) (colors-c3 (game-colors g)) (colors-c4 (game-colors g))
      (colors-c5 (game-colors g)) (colors-c6 (game-colors g)) (colors-c7 (game-colors g)) (colors-c8 (game-colors g))
      (updateGameScore (game-score g) 50) (colors-button3 (game-colors g)))(updateGameScore (game-score g) 50) (game-menu g) (game-gameOver g) "colors"))
                                         ((isCollided (colors-f2 (game-colors g))(colors-c2 (game-colors g)) 30) (make-game (game-shapes g) (game-numbers g) (make-colors
      (colors-f1 (game-colors g)) (make-obj (obj-im (colors-f2 (game-colors g)))(make-pos -200 -200)) (colors-f3 (game-colors g)) (colors-f4 (game-colors g))
      (colors-f5 (game-colors g)) (colors-f6 (game-colors g)) (colors-f7 (game-colors g)) (colors-f8 (game-colors g))
      (colors-c1 (game-colors g)) (colors-c2 (game-colors g)) (colors-c3 (game-colors g)) (colors-c4 (game-colors g))
      (colors-c5 (game-colors g)) (colors-c6 (game-colors g)) (colors-c7 (game-colors g)) (colors-c8 (game-colors g))
      (updateGameScore (game-score g) 50) (colors-button3 (game-colors g)))(updateGameScore (game-score g) 50) (game-menu g) (game-gameOver g) "colors"))
                                         ((isCollided (colors-f3 (game-colors g))(colors-c3 (game-colors g)) 30) (make-game (game-shapes g) (game-numbers g) (make-colors
      (colors-f1 (game-colors g)) (colors-f2 (game-colors g)) (make-obj (obj-im (colors-f3 (game-colors g)))(make-pos -200 -200)) (colors-f4 (game-colors g))
      (colors-f5 (game-colors g)) (colors-f6 (game-colors g)) (colors-f7 (game-colors g)) (colors-f8 (game-colors g))
      (colors-c1 (game-colors g)) (colors-c2 (game-colors g)) (colors-c3 (game-colors g)) (colors-c4 (game-colors g))
      (colors-c5 (game-colors g)) (colors-c6 (game-colors g)) (colors-c7 (game-colors g)) (colors-c8 (game-colors g))
      (updateGameScore (game-score g) 50) (colors-button3 (game-colors g)))(updateGameScore (game-score g) 50) (game-menu g) (game-gameOver g) "colors"))
                                         ((isCollided (colors-f4 (game-colors g))(colors-c4 (game-colors g)) 10) (make-game (game-shapes g) (game-numbers g) (make-colors
      (colors-f1 (game-colors g)) (colors-f2 (game-colors g)) (colors-f3 (game-colors g)) (make-obj (obj-im (colors-f4 (game-colors g)))(make-pos -200 -200))
      (colors-f5 (game-colors g)) (colors-f6 (game-colors g)) (colors-f7 (game-colors g)) (colors-f8 (game-colors g))
      (colors-c1 (game-colors g)) (colors-c2 (game-colors g)) (colors-c3 (game-colors g)) (colors-c4 (game-colors g))
      (colors-c5 (game-colors g)) (colors-c6 (game-colors g)) (colors-c7 (game-colors g)) (colors-c8 (game-colors g))
      (updateGameScore (game-score g) 50) (colors-button3 (game-colors g)))(updateGameScore (game-score g) 50) (game-menu g) (game-gameOver g) "colors"))
                                         ((isCollided (colors-f5 (game-colors g))(colors-c5 (game-colors g)) 30) (make-game (game-shapes g) (game-numbers g) (make-colors
      (colors-f1 (game-colors g)) (colors-f2 (game-colors g)) (colors-f3 (game-colors g)) (colors-f4 (game-colors g))
      (make-obj (obj-im (colors-f5 (game-colors g)))(make-pos -200 -200)) (colors-f6 (game-colors g)) (colors-f7 (game-colors g)) (colors-f8 (game-colors g))
      (colors-c1 (game-colors g)) (colors-c2 (game-colors g)) (colors-c3 (game-colors g)) (colors-c4 (game-colors g))
      (colors-c5 (game-colors g)) (colors-c6 (game-colors g)) (colors-c7 (game-colors g)) (colors-c8 (game-colors g))
      (updateGameScore (game-score g) 50) (colors-button3 (game-colors g)))(updateGameScore (game-score g) 50) (game-menu g) (game-gameOver g) "colors"))
                                         ((isCollided (colors-f6 (game-colors g))(colors-c6 (game-colors g)) 30) (make-game (game-shapes g) (game-numbers g) (make-colors
      (colors-f1 (game-colors g)) (colors-f2 (game-colors g)) (colors-f3 (game-colors g)) (colors-f4 (game-colors g))
      (colors-f5 (game-colors g)) (make-obj (obj-im (colors-f6 (game-colors g)))(make-pos -200 -200)) (colors-f7 (game-colors g)) (colors-f8 (game-colors g))
      (colors-c1 (game-colors g)) (colors-c2 (game-colors g)) (colors-c3 (game-colors g)) (colors-c4 (game-colors g))
      (colors-c5 (game-colors g)) (colors-c6 (game-colors g)) (colors-c7 (game-colors g)) (colors-c8 (game-colors g))
      (updateGameScore (game-score g) 50) (colors-button3 (game-colors g)))(updateGameScore (game-score g) 50) (game-menu g) (game-gameOver g) "colors"))
                                         ((isCollided (colors-f7 (game-colors g))(colors-c7 (game-colors g)) 30) (make-game (game-shapes g) (game-numbers g) (make-colors
      (colors-f1 (game-colors g)) (colors-f2 (game-colors g)) (colors-f3 (game-colors g)) (colors-f4 (game-colors g))
      (colors-f5 (game-colors g)) (colors-f6 (game-colors g)) (make-obj (obj-im (colors-f7 (game-colors g)))(make-pos -200 -200)) (colors-f8 (game-colors g))
      (colors-c1 (game-colors g)) (colors-c2 (game-colors g)) (colors-c3 (game-colors g)) (colors-c4 (game-colors g))
      (colors-c5 (game-colors g)) (colors-c6 (game-colors g)) (colors-c7 (game-colors g)) (colors-c8 (game-colors g))
      (updateGameScore (game-score g) 50) (colors-button3 (game-colors g)))(updateGameScore (game-score g) 50) (game-menu g) (game-gameOver g) "colors"))
                                         ((isCollided (colors-f8 (game-colors g))(colors-c8 (game-colors g)) 30) (make-game (game-shapes g) (game-numbers g) (make-colors
      (colors-f1 (game-colors g)) (colors-f2 (game-colors g)) (colors-f3 (game-colors g)) (colors-f4 (game-colors g))
      (colors-f5 (game-colors g)) (colors-f6 (game-colors g)) (colors-f7 (game-colors g)) (make-obj (obj-im (colors-f8 (game-colors g)))(make-pos -200 -200))
      (colors-c1 (game-colors g)) (colors-c2 (game-colors g)) (colors-c3 (game-colors g)) (colors-c4 (game-colors g))
      (colors-c5 (game-colors g)) (colors-c6 (game-colors g)) (colors-c7 (game-colors g)) (colors-c8 (game-colors g))
      (updateGameScore (game-score g) 50) (colors-button3 (game-colors g)))(updateGameScore (game-score g) 50) (game-menu g) (game-gameOver g) "colors"))
                                         
                                         ((and (mouse=? event "drag") (isInObject (colors-f1 (game-colors g)) x y)) (make-game (game-shapes g) (game-numbers g) (make-colors
      (make-obj (obj-im (colors-f1 (game-colors g)))(make-pos x y)) (colors-f2 (game-colors g)) (colors-f3 (game-colors g)) (colors-f4 (game-colors g))
      (colors-f5 (game-colors g)) (colors-f6 (game-colors g)) (colors-f7 (game-colors g)) (colors-f8 (game-colors g))
      (colors-c1 (game-colors g)) (colors-c2 (game-colors g)) (colors-c3 (game-colors g)) (colors-c4 (game-colors g))
      (colors-c5 (game-colors g)) (colors-c6 (game-colors g)) (colors-c7 (game-colors g)) (colors-c8 (game-colors g))
      (game-score g) (colors-button3 (game-colors g)))
      (game-score g) (game-menu g) (game-gameOver g) (game-current_scene g)))
                                         ((and (mouse=? event "drag") (isInObject (colors-f2 (game-colors g)) x y)) (make-game (game-shapes g) (game-numbers g) (make-colors
      (colors-f1 (game-colors g)) (make-obj (obj-im (colors-f2 (game-colors g)))(make-pos x y)) (colors-f3 (game-colors g)) (colors-f4 (game-colors g))
      (colors-f5 (game-colors g)) (colors-f6 (game-colors g)) (colors-f7 (game-colors g)) (colors-f8 (game-colors g))
      (colors-c1 (game-colors g)) (colors-c2 (game-colors g)) (colors-c3 (game-colors g)) (colors-c4 (game-colors g))
      (colors-c5 (game-colors g)) (colors-c6 (game-colors g)) (colors-c7 (game-colors g)) (colors-c8 (game-colors g))
      (game-score g) (colors-button3 (game-colors g)))
      (game-score g) (game-menu g) (game-gameOver g) (game-current_scene g)))
                                         ((and (mouse=? event "drag") (isInObject (colors-f3 (game-colors g)) x y)) (make-game (game-shapes g) (game-numbers g) (make-colors
      (colors-f1 (game-colors g)) (colors-f2 (game-colors g)) (make-obj (obj-im (colors-f3 (game-colors g)))(make-pos x y)) (colors-f4 (game-colors g))
      (colors-f5 (game-colors g)) (colors-f6 (game-colors g)) (colors-f7 (game-colors g)) (colors-f8 (game-colors g))
      (colors-c1 (game-colors g)) (colors-c2 (game-colors g)) (colors-c3 (game-colors g)) (colors-c4 (game-colors g))
      (colors-c5 (game-colors g)) (colors-c6 (game-colors g)) (colors-c7 (game-colors g)) (colors-c8 (game-colors g))
      (game-score g) (colors-button3 (game-colors g)))
      (game-score g) (game-menu g) (game-gameOver g) (game-current_scene g)))
                                         ((and (mouse=? event "drag") (isInObject (colors-f4 (game-colors g)) x y)) (make-game (game-shapes g) (game-numbers g) (make-colors
      (colors-f1 (game-colors g)) (colors-f2 (game-colors g)) (colors-f3 (game-colors g)) (make-obj (obj-im (colors-f4 (game-colors g)))(make-pos x y))
      (colors-f5 (game-colors g)) (colors-f6 (game-colors g)) (colors-f7 (game-colors g)) (colors-f8 (game-colors g))
      (colors-c1 (game-colors g)) (colors-c2 (game-colors g)) (colors-c3 (game-colors g)) (colors-c4 (game-colors g))
      (colors-c5 (game-colors g)) (colors-c6 (game-colors g)) (colors-c7 (game-colors g)) (colors-c8 (game-colors g))
      (game-score g) (colors-button3 (game-colors g)))
      (game-score g) (game-menu g) (game-gameOver g) (game-current_scene g)))
                                         ((and (mouse=? event "drag") (isInObject (colors-f5 (game-colors g)) x y)) (make-game (game-shapes g) (game-numbers g) (make-colors
      (colors-f1 (game-colors g)) (colors-f2 (game-colors g)) (colors-f3 (game-colors g)) (colors-f4 (game-colors g))
      (make-obj (obj-im (colors-f5 (game-colors g)))(make-pos x y)) (colors-f6 (game-colors g)) (colors-f7 (game-colors g)) (colors-f8 (game-colors g))
      (colors-c1 (game-colors g)) (colors-c2 (game-colors g)) (colors-c3 (game-colors g)) (colors-c4 (game-colors g))
      (colors-c5 (game-colors g)) (colors-c6 (game-colors g)) (colors-c7 (game-colors g)) (colors-c8 (game-colors g))
      (game-score g) (colors-button3 (game-colors g)))
      (game-score g) (game-menu g) (game-gameOver g) (game-current_scene g)))
                                         ((and (mouse=? event "drag") (isInObject (colors-f6 (game-colors g)) x y)) (make-game (game-shapes g) (game-numbers g) (make-colors
      (colors-f1 (game-colors g)) (colors-f2 (game-colors g)) (colors-f3 (game-colors g)) (colors-f4 (game-colors g))
      (colors-f5 (game-colors g)) (make-obj (obj-im (colors-f6 (game-colors g)))(make-pos x y)) (colors-f7 (game-colors g)) (colors-f8 (game-colors g))
      (colors-c1 (game-colors g)) (colors-c2 (game-colors g)) (colors-c3 (game-colors g)) (colors-c4 (game-colors g))
      (colors-c5 (game-colors g)) (colors-c6 (game-colors g)) (colors-c7 (game-colors g)) (colors-c8 (game-colors g))
      (game-score g) (colors-button3 (game-colors g)))
      (game-score g) (game-menu g) (game-gameOver g) (game-current_scene g)))
                                         ((and (mouse=? event "drag") (isInObject (colors-f7 (game-colors g)) x y)) (make-game (game-shapes g) (game-numbers g) (make-colors
      (colors-f1 (game-colors g)) (colors-f2 (game-colors g)) (colors-f3 (game-colors g)) (colors-f4 (game-colors g))
      (colors-f5 (game-colors g)) (colors-f6 (game-colors g)) (make-obj (obj-im (colors-f7 (game-colors g)))(make-pos x y)) (colors-f8 (game-colors g))
      (colors-c1 (game-colors g)) (colors-c2 (game-colors g)) (colors-c3 (game-colors g)) (colors-c4 (game-colors g))
      (colors-c5 (game-colors g)) (colors-c6 (game-colors g)) (colors-c7 (game-colors g)) (colors-c8 (game-colors g))
      (game-score g) (colors-button3 (game-colors g)))
      (game-score g) (game-menu g) (game-gameOver g) (game-current_scene g)))
                                         ((and (mouse=? event "drag") (isInObject (colors-f8 (game-colors g)) x y)) (make-game (game-shapes g) (game-numbers g) (make-colors
      (colors-f1 (game-colors g)) (colors-f2 (game-colors g)) (colors-f3 (game-colors g)) (colors-f4 (game-colors g))
      (colors-f5 (game-colors g)) (colors-f6 (game-colors g)) (colors-f7 (game-colors g)) (make-obj (obj-im (colors-f8 (game-colors g)))(make-pos x y)) 
      (colors-c1 (game-colors g)) (colors-c2 (game-colors g)) (colors-c3 (game-colors g)) (colors-c4 (game-colors g))
      (colors-c5 (game-colors g)) (colors-c6 (game-colors g)) (colors-c7 (game-colors g)) (colors-c8 (game-colors g))
      (game-score g) (colors-button3 (game-colors g)))
      (game-score g) (game-menu g) (game-gameOver g) (game-current_scene g)))
    (else g)))

;-------------------------draw functions--------------------------
; purpose: draw the game to the scene
; contract: drawGame: g(obj) -> image
; function
(define (drawGame g)
  (cond
    ((string=? (game-current_scene g) "menu") (drawMenu (game-menu g)))   ; Draw the menu scene
    ((string=? (game-current_scene g) "shapes") (drawShapes (game-shapes g)))   ; Draw the shapes scene
    ((string=? (game-current_scene g) "numbers") (drawNumbers (game-numbers g)))   ; Draw the numbers scene
    ((string=? (game-current_scene g) "colors") (drawColors (game-colors g)))   ; Draw the colors scene
    ((string=? (game-current_scene g) "gameOver") (drawGameOver (game-gameOver g)))   ; Draw the gameOver scene
    (else (empty-scene 800 800))))

; purpose: Draw the menu scene including the startGame button and associated text.
; contract: drawMenu: m(menu) -> image
; test:
(check-expect (drawMenu (make-menu (make-obj (circle 50 "solid" "red") (make-pos 100 100)) (make-obj (circle 50 "solid" "red") (make-pos 200 200))))
              (place-image (circle 50 "solid" "red") 100 100
                           (place-image (text/font "Playful Learning" 55 "steelblue" "modern" 'swiss 'normal 'bold #f) 400 170
                                        (place-image (circle 50 "solid" "red") 200 200
                                        (empty-scene 800 800)))))
; function:
(define (drawMenu m) (place-image (obj-im (menu-startGame m)) (pos-x (obj-pos (menu-startGame m))) (pos-y (obj-pos (menu-startGame m)))
                                  (place-image (text/font "Playful Learning"  55 "steelblue" "modern" 'swiss 'normal 'bold #f) 400 170
                                                (place-image (obj-im (menu-background m)) (pos-x (obj-pos (menu-background m))) (pos-y (obj-pos (menu-background m)))
                                  (empty-scene 800 800)))))

; purpose: draw the game over scene including a congratulatory message and two images.
; contract: drawGameOver: g(gameOver) -> image
; test:
(check-expect (drawGameOver (make-gameOver "You completed the game"
                                           (make-obj (circle 50 "solid" "red") (make-pos 200 200))
                                           (make-obj (circle 60 "solid" "blue") (make-pos 300 300))))
              (place-image (text/font "You completed the game"  35 "dodgerblue" "modern" 'swiss 'normal 'bold #f) 400 400
                           (place-image (circle 50 "solid" "red") 200 200
                                        (place-image (circle 60 "solid" "blue") 300 300
                                                     (empty-scene 800 800)))))
; function:
(define (drawGameOver g) (place-image (text/font "You completed the game"  35 "dodgerblue" "modern" 'swiss 'normal 'bold #f) 400 400
                                      (place-image (obj-im (gameOver-balloon1 g)) (pos-x (obj-pos (gameOver-balloon1 g))) (pos-y (obj-pos (gameOver-balloon1 g)))
                                                   (place-image (obj-im (gameOver-balloon2 g)) (pos-x (obj-pos (gameOver-balloon2 g))) (pos-y (obj-pos (gameOver-balloon2 g)))
                                      (empty-scene 800 800)))))
; purpose: draw the shapes scene
; contract: drawShapes: g(shapes) -> image
; test:
(check-expect (drawShapes (make-shapes CIRCLE MAINCIRCLE MAINSQUARE MAINTRIANGLE SQUARE TRIANGLE 0 BUTTON))
              (place-image (obj-im CIRCLE) (pos-x (obj-pos CIRCLE)) (pos-y (obj-pos CIRCLE))
                           (place-image (obj-im MAINCIRCLE) (pos-x (obj-pos MAINCIRCLE)) (pos-y (obj-pos MAINCIRCLE))
                                        (place-image (obj-im MAINSQUARE) (pos-x (obj-pos MAINSQUARE)) (pos-y (obj-pos MAINSQUARE))
                                                     (place-image (obj-im MAINTRIANGLE) (pos-x (obj-pos MAINTRIANGLE)) (pos-y (obj-pos MAINTRIANGLE))
                                                                  (place-image (obj-im SQUARE) (pos-x (obj-pos SQUARE)) (pos-y (obj-pos SQUARE))
                                                                               (place-image (obj-im TRIANGLE) (pos-x (obj-pos TRIANGLE)) (pos-y (obj-pos TRIANGLE))
                                                                                            (place-image (text (string-append "Score: 0") 25 "orange") 720 33
                                                                                                         (place-image (obj-im BUTTON) (pos-x (obj-pos BUTTON)) (pos-y (obj-pos BUTTON))
                                                                                                                      (empty-scene 800 800))))))))))
; function: 
(define (drawShapes g)  (place-image (obj-im (shapes-circle g)) (pos-x (obj-pos (shapes-circle g))) (pos-y (obj-pos (shapes-circle g)))
                        (place-image (obj-im (shapes-mainCircle g)) (pos-x (obj-pos (shapes-mainCircle g))) (pos-y (obj-pos (shapes-mainCircle g)))
                        (place-image (obj-im (shapes-mainSquare g)) (pos-x (obj-pos (shapes-mainSquare g))) (pos-y (obj-pos (shapes-mainSquare g)))
                        (place-image (obj-im (shapes-mainTriangle g)) (pos-x (obj-pos (shapes-mainTriangle g))) (pos-y (obj-pos (shapes-mainTriangle g)))
                        (place-image (obj-im (shapes-square g)) (pos-x (obj-pos (shapes-square g))) (pos-y (obj-pos (shapes-square g)))
                        (place-image (obj-im (shapes-triangle g)) (pos-x (obj-pos (shapes-triangle g))) (pos-y (obj-pos (shapes-triangle g)))
                        (place-image (text (string-append "Score: " (number->string (shapes-score g))) 25 "orange") 720 33
                        (place-image (obj-im (shapes-button g)) (pos-x (obj-pos (shapes-button g))) (pos-y (obj-pos (shapes-button g)))
                                     (empty-scene 800 800)))))))))) 

; purpose: draw the numbers scene
; contract: drawNumbers: g(numbers) -> image
; test:
(check-expect
  (drawNumbers (make-numbers n0 n1 n2 n3 n4 n5 n6 n7 n8 n9 star0 star1 star2 star3 star4 star5 star6 star7 star8 star9 0 BUTTON2))
  (place-image (obj-im n0) (pos-x (obj-pos n0)) (pos-y (obj-pos n0))
    (place-image (obj-im n1) (pos-x (obj-pos n1)) (pos-y (obj-pos n1))
      (place-image (obj-im n2) (pos-x (obj-pos n2)) (pos-y (obj-pos n2))
        (place-image (obj-im n3) (pos-x (obj-pos n3)) (pos-y (obj-pos n3))
          (place-image (obj-im n4) (pos-x (obj-pos n4)) (pos-y (obj-pos n4))
            (place-image (obj-im n5) (pos-x (obj-pos n5)) (pos-y (obj-pos n5))
              (place-image (obj-im n6) (pos-x (obj-pos n6)) (pos-y (obj-pos n6))
                (place-image (obj-im n7) (pos-x (obj-pos n7)) (pos-y (obj-pos n7))
                  (place-image (obj-im n8) (pos-x (obj-pos n8)) (pos-y (obj-pos n8))
                    (place-image (obj-im n9) (pos-x (obj-pos n9)) (pos-y (obj-pos n9))
                      (place-image (obj-im star0) (pos-x (obj-pos star0)) (pos-y (obj-pos star0))
                      (place-image (obj-im star1) (pos-x (obj-pos star1)) (pos-y (obj-pos star1))
                      (place-image (obj-im star2) (pos-x (obj-pos star2)) (pos-y (obj-pos star2))
                      (place-image (obj-im star3) (pos-x (obj-pos star3)) (pos-y (obj-pos star3))
                      (place-image (obj-im star4) (pos-x (obj-pos star4)) (pos-y (obj-pos star4))
                      (place-image (obj-im star5) (pos-x (obj-pos star5)) (pos-y (obj-pos star5))
                      (place-image (obj-im star6) (pos-x (obj-pos star6)) (pos-y (obj-pos star6))
                      (place-image (obj-im star7) (pos-x (obj-pos star7)) (pos-y (obj-pos star7))
                      (place-image (obj-im star8) (pos-x (obj-pos star8)) (pos-y (obj-pos star8))
                      (place-image (obj-im star9) (pos-x (obj-pos star9)) (pos-y (obj-pos star9))
                      (place-image (text (string-append "Score: 0") 25 "orange") 720 33
                      (place-image (obj-im BUTTON2) (pos-x (obj-pos BUTTON2)) (pos-y (obj-pos BUTTON2))
                      (empty-scene 800 800))))))))))))))))))))))))

; function:
(define (drawNumbers g) (place-image (obj-im (numbers-n0 g)) (pos-x (obj-pos (numbers-n0 g))) (pos-y (obj-pos (numbers-n0 g)))   (place-image (obj-im (numbers-star0 g)) (pos-x (obj-pos (numbers-star0 g))) (pos-y (obj-pos (numbers-star0 g)))
                           (place-image (obj-im (numbers-n1 g)) (pos-x (obj-pos (numbers-n1 g))) (pos-y (obj-pos (numbers-n1 g)))   (place-image (obj-im (numbers-star1 g)) (pos-x (obj-pos (numbers-star1 g))) (pos-y (obj-pos (numbers-star1 g)))
                              (place-image (obj-im (numbers-n2 g)) (pos-x (obj-pos (numbers-n2 g))) (pos-y (obj-pos (numbers-n2 g)))   (place-image (obj-im (numbers-star2 g)) (pos-x (obj-pos (numbers-star2 g))) (pos-y (obj-pos (numbers-star2 g)))
                                 (place-image (obj-im (numbers-n3 g)) (pos-x (obj-pos (numbers-n3 g))) (pos-y (obj-pos (numbers-n3 g)))   (place-image (obj-im (numbers-star3 g)) (pos-x (obj-pos (numbers-star3 g))) (pos-y (obj-pos (numbers-star3 g)))
                                    (place-image (obj-im (numbers-n4 g)) (pos-x (obj-pos (numbers-n4 g))) (pos-y (obj-pos (numbers-n4 g)))   (place-image (obj-im (numbers-star4 g)) (pos-x (obj-pos (numbers-star4 g))) (pos-y (obj-pos (numbers-star4 g)))
                                       (place-image (obj-im (numbers-n5 g)) (pos-x (obj-pos (numbers-n5 g))) (pos-y (obj-pos (numbers-n5 g)))   (place-image (obj-im (numbers-star5 g)) (pos-x (obj-pos (numbers-star5 g))) (pos-y (obj-pos (numbers-star5 g)))
                                          (place-image (obj-im (numbers-n6 g)) (pos-x (obj-pos (numbers-n6 g))) (pos-y (obj-pos (numbers-n6 g)))   (place-image (obj-im (numbers-star6 g)) (pos-x (obj-pos (numbers-star6 g))) (pos-y (obj-pos (numbers-star6 g)))
                                             (place-image (obj-im (numbers-n7 g)) (pos-x (obj-pos (numbers-n7 g))) (pos-y (obj-pos (numbers-n7 g)))   (place-image (obj-im (numbers-star7 g)) (pos-x (obj-pos (numbers-star7 g))) (pos-y (obj-pos (numbers-star7 g)))
                                                (place-image (obj-im (numbers-n8 g)) (pos-x (obj-pos (numbers-n8 g))) (pos-y (obj-pos (numbers-n8 g)))   (place-image (obj-im (numbers-star8 g)) (pos-x (obj-pos (numbers-star8 g))) (pos-y (obj-pos (numbers-star8 g)))
                                                   (place-image (obj-im (numbers-n9 g)) (pos-x (obj-pos (numbers-n9 g))) (pos-y (obj-pos (numbers-n9 g)))   (place-image (obj-im (numbers-star9 g)) (pos-x (obj-pos (numbers-star9 g))) (pos-y (obj-pos (numbers-star9 g)))
                                                      (place-image (text (string-append "Score: " (number->string (numbers-score g))) 25 "orange") 720 33
                                                      (place-image (obj-im (numbers-button2 g)) (pos-x (obj-pos (numbers-button2 g))) (pos-y (obj-pos (numbers-button2 g)))
                                                      (empty-scene 800 800))))))))))))))))))))))))

; purpose: draw the colors scene
; contract: drawColors: g(colors) -> image
; test:
(check-expect
  (drawColors (make-colors c1 c2 c3 c4 c5 c6 c7 c8 f1 f2 f3 f4 f5 f6 f7 f8 0 BUTTON3))
  (place-image (obj-im c1) (pos-x (obj-pos c1)) (pos-y (obj-pos c1))
    (place-image (obj-im c2) (pos-x (obj-pos c2)) (pos-y (obj-pos c2))
      (place-image (obj-im c3) (pos-x (obj-pos c3)) (pos-y (obj-pos c3))
        (place-image (obj-im c4) (pos-x (obj-pos c4)) (pos-y (obj-pos c4))
          (place-image (obj-im c5) (pos-x (obj-pos c5)) (pos-y (obj-pos c5))
            (place-image (obj-im c6) (pos-x (obj-pos c6)) (pos-y (obj-pos c6))
              (place-image (obj-im c7) (pos-x (obj-pos c7)) (pos-y (obj-pos c7))
                (place-image (obj-im c8) (pos-x (obj-pos c8)) (pos-y (obj-pos c8))
                  (place-image (obj-im f1) (pos-x (obj-pos f1)) (pos-y (obj-pos f1))
                    (place-image (obj-im f2) (pos-x (obj-pos f2)) (pos-y (obj-pos f2))
                      (place-image (obj-im f3) (pos-x (obj-pos f3)) (pos-y (obj-pos f3))
                        (place-image (obj-im f4) (pos-x (obj-pos f4)) (pos-y (obj-pos f4))
                          (place-image (obj-im f5) (pos-x (obj-pos f5)) (pos-y (obj-pos f5))
                            (place-image (obj-im f6) (pos-x (obj-pos f6)) (pos-y (obj-pos f6))
                              (place-image (obj-im f7) (pos-x (obj-pos f7)) (pos-y (obj-pos f7))
                                (place-image (obj-im f8) (pos-x (obj-pos f8)) (pos-y (obj-pos f8))
                                  (place-image (text (string-append "Score: 0") 25 "orange") 720 33
                                    (place-image (obj-im BUTTON3) (pos-x (obj-pos BUTTON3)) (pos-y (obj-pos BUTTON3))
                                      (empty-scene 800 800))))))))))))))))))))

; function:
(define (drawColors g) (place-image (obj-im (colors-f1 g)) (pos-x (obj-pos (colors-f1 g))) (pos-y (obj-pos (colors-f1 g)))   (place-image (obj-im (colors-c1 g)) (pos-x (obj-pos (colors-c1 g))) (pos-y (obj-pos (colors-c1 g)))
                           (place-image (obj-im (colors-f2 g)) (pos-x (obj-pos (colors-f2 g))) (pos-y (obj-pos (colors-f2 g)))   (place-image (obj-im (colors-c2 g)) (pos-x (obj-pos (colors-c2 g))) (pos-y (obj-pos (colors-c2 g)))
                              (place-image (obj-im (colors-f3 g)) (pos-x (obj-pos (colors-f3 g))) (pos-y (obj-pos (colors-f3 g)))   (place-image (obj-im (colors-c3 g)) (pos-x (obj-pos (colors-c3 g))) (pos-y (obj-pos (colors-c3 g)))
                                 (place-image (obj-im (colors-f4 g)) (pos-x (obj-pos (colors-f4 g))) (pos-y (obj-pos (colors-f4 g)))   (place-image (obj-im (colors-c4 g)) (pos-x (obj-pos (colors-c4 g))) (pos-y (obj-pos (colors-c4 g)))
                                    (place-image (obj-im (colors-f5 g)) (pos-x (obj-pos (colors-f5 g))) (pos-y (obj-pos (colors-f5 g)))   (place-image (obj-im (colors-c5 g)) (pos-x (obj-pos (colors-c5 g))) (pos-y (obj-pos (colors-c5 g)))
                                       (place-image (obj-im (colors-f6 g)) (pos-x (obj-pos (colors-f6 g))) (pos-y (obj-pos (colors-f6 g)))   (place-image (obj-im (colors-c6 g)) (pos-x (obj-pos (colors-c6 g))) (pos-y (obj-pos (colors-c6 g)))
                                          (place-image (obj-im (colors-f7 g)) (pos-x (obj-pos (colors-f7 g))) (pos-y (obj-pos (colors-f7 g)))   (place-image (obj-im (colors-c7 g)) (pos-x (obj-pos (colors-c7 g))) (pos-y (obj-pos (colors-c7 g)))
                                             (place-image (obj-im (colors-f8 g)) (pos-x (obj-pos (colors-f8 g))) (pos-y (obj-pos (colors-f8 g)))   (place-image (obj-im (colors-c8 g)) (pos-x (obj-pos (colors-c8 g))) (pos-y (obj-pos (colors-c8 g)))
                                                      (place-image (text (string-append "Score: " (number->string (colors-score g))) 25 "orange") 720 33
                                                      (place-image (obj-im (colors-button3 g)) (pos-x (obj-pos (colors-button3 g))) (pos-y (obj-pos (colors-button3 g))) 
                                                      (empty-scene 800 800))))))))))))))))))))

; define functions for menu scene
(define MENU (make-obj (polygon (list (make-posn -5 0)(make-posn -5 70)(make-posn 140 0)(make-posn -5 -70))"solid" "gray") (make-pos 400 400)))
(define background-image (bitmap "background.png")) (define background-pos (make-pos 400 430)) (define background (make-obj background-image background-pos))

; define functions for shapes scene
(define CIRCLE (make-obj (circle 50 "solid" "blue")  (make-pos 400 500)))
(define MAINCIRCLE (make-obj (circle 75 "solid" "red") (make-pos 160 225)))
(define MAINSQUARE (make-obj (square 150 "solid" "red") (make-pos 640 225)))
(define MAINTRIANGLE (make-obj (triangle 150 "solid" "red") (make-pos 400 650)))
(define SQUARE (make-obj (square 100 "solid" "blue") (make-pos 400 375)))
(define TRIANGLE (make-obj (triangle 100 "solid" "blue") (make-pos 400 250)))
(define BUTTON (make-obj (polygon (list (make-posn -5 0)(make-posn -5 35)(make-posn 70 0)(make-posn -5 -35))"solid" "gray") (make-pos 620 670)))

; define functions for numbers scene
(define n0-image (bitmap "n0.png")) (define n0-pos (make-pos 70 120)) (define n0 (make-obj n0-image n0-pos))
(define n1-image (bitmap "n1.png")) (define n1-pos (make-pos 70 265)) (define n1 (make-obj n1-image n1-pos))
(define n2-image (bitmap "n2.png")) (define n2-pos (make-pos 70 400)) (define n2 (make-obj n2-image n2-pos))
(define n3-image (bitmap "n3.png")) (define n3-pos (make-pos 70 535)) (define n3 (make-obj n3-image n3-pos))
(define n4-image (bitmap "n4.png")) (define n4-pos (make-pos 75 670)) (define n4 (make-obj n4-image n4-pos))
(define n5-image (bitmap "n5.png")) (define n5-pos (make-pos 730 130)) (define n5 (make-obj n5-image n5-pos))
(define n6-image (bitmap "n6.png")) (define n6-pos (make-pos 730 265)) (define n6 (make-obj n6-image n6-pos))
(define n7-image (bitmap "n7.png")) (define n7-pos (make-pos 730 400)) (define n7 (make-obj n7-image n7-pos))
(define n8-image (bitmap "n8.png")) (define n8-pos (make-pos 725 530)) (define n8 (make-obj n8-image n8-pos))
(define n9-image (bitmap "n9.png")) (define n9-pos (make-pos 720 660)) (define n9 (make-obj n9-image n9-pos))
(define STAR (star 22 "solid" "goldenrod"))
(define BIGCIRCLE (circle 70 "solid" "lightblue"))
(define star0 (make-obj BIGCIRCLE (make-pos 475 550)))
(define star1 (make-obj (overlay STAR BIGCIRCLE) (make-pos 475 250)))
(define star2 (make-obj (overlay(beside STAR STAR) BIGCIRCLE) (make-pos 475 700)))
(define star3 (make-obj (overlay(above (beside STAR STAR) STAR) BIGCIRCLE) (make-pos 325 550)))
(define star4 (make-obj (overlay(above (beside STAR STAR) (beside STAR STAR)) BIGCIRCLE) (make-pos 325 250)))
(define star5 (make-obj (overlay(overlay(above (beside STAR STAR)(beside STAR STAR)) STAR) BIGCIRCLE) (make-pos 325 400)))
(define star6 (make-obj (overlay(above (beside STAR STAR STAR)(beside STAR STAR STAR)) BIGCIRCLE) (make-pos 325 700)))
(define star7 (make-obj (overlay(above (beside STAR STAR STAR) (beside STAR STAR STAR) STAR) BIGCIRCLE) (make-pos 475 400)))
(define star8 (make-obj (overlay(above (above (beside STAR STAR STAR) (beside STAR STAR STAR)) (beside STAR STAR)) BIGCIRCLE) (make-pos 325 100)))
(define star9 (make-obj (overlay(above (beside STAR STAR STAR)(beside STAR STAR STAR)(beside STAR STAR STAR)) BIGCIRCLE) (make-pos 475 100)))
(define BUTTON2 (make-obj (polygon (list (make-posn -5 0)(make-posn -5 35)(make-posn 70 0)(make-posn -5 -35))"solid" "gray") (make-pos 650 745)))

; define functions for colors scene
(define c1 (make-obj (overlay (text/font "RED"  15 "white" "modern" 'swiss 'normal 'bold #f) (square 100 "solid" "red")) (make-pos 350 250)))
(define c2 (make-obj (overlay (text/font "YELLOW"  15 "white" "modern" 'swiss 'normal 'bold #f)(square 100 "solid" "yellow"))  (make-pos 450 250)))
(define c3 (make-obj (overlay (text/font "ORANGE"  15 "white" "modern" 'swiss 'normal 'bold #f)(square 100 "solid" "orange"))  (make-pos 350 350)))
(define c4 (make-obj (overlay (text/font "GREEN"  15 "white" "modern" 'swiss 'normal 'bold #f)(square 100 "solid" "lawngreen"))  (make-pos 450 350)))
(define c5 (make-obj (overlay (text/font "BLUE"  15 "white" "modern" 'swiss 'normal 'bold #f)(square 100 "solid" "blue"))  (make-pos 350 450)))
(define c6 (make-obj (overlay (text/font "PURPLE"  15 "white" "modern" 'swiss 'normal 'bold #f)(square 100 "solid" "purple"))  (make-pos 450 450)))
(define c7 (make-obj (overlay (text/font "BROWN"  15 "white" "modern" 'swiss 'normal 'bold #f)(square 100 "solid" "brown"))  (make-pos 350 550)))
(define c8 (make-obj (overlay (text/font "PINK"  15 "white" "modern" 'swiss 'normal 'bold #f)(square 100 "solid" "hotpink"))  (make-pos 450 550)))
(define f1-image (bitmap "f1.png")) (define f1-pos (make-pos 80 650)) (define f1 (make-obj f1-image f1-pos))
(define f2-image (bitmap "f2.png")) (define f2-pos (make-pos 80 316)) (define f2 (make-obj f2-image f2-pos))
(define f3-image (bitmap "f3.png")) (define f3-pos (make-pos 720 150)) (define f3 (make-obj f3-image f3-pos))
(define f4-image (bitmap "f4.png")) (define f4-pos (make-pos 720 482)) (define f4 (make-obj f4-image f4-pos))
(define f5-image (bitmap "f5.png")) (define f5-pos (make-pos 80 482)) (define f5 (make-obj f5-image f5-pos))
(define f6-image (bitmap "f6.png")) (define f6-pos (make-pos 80 150)) (define f6 (make-obj f6-image f6-pos))
(define f7-image (bitmap "f7.png")) (define f7-pos (make-pos 720 316)) (define f7 (make-obj f7-image f7-pos))
(define f8-image (bitmap "f8.png")) (define f8-pos (make-pos 720 650)) (define f8 (make-obj f8-image f8-pos))
(define BUTTON3 (make-obj (polygon (list (make-posn -5 0)(make-posn -5 35)(make-posn 70 0)(make-posn -5 -35))"solid" "gray") (make-pos 600 730)))

; define functions for gameOver scene
(define balloon1-image (bitmap "balloons1.png")) (define balloon1-pos (make-pos 600 400)) (define balloon1 (make-obj balloon1-image balloon1-pos))
(define balloon2-image (bitmap "balloons2.png")) (define balloon2-pos (make-pos 200 400)) (define balloon2 (make-obj balloon2-image balloon2-pos))

; big-bang
(define SHAPES (make-shapes CIRCLE MAINCIRCLE MAINSQUARE MAINTRIANGLE SQUARE TRIANGLE 0 BUTTON))

(define NUMBERS (make-numbers n0 n1 n2 n3 n4 n5 n6 n7 n8 n9
                              star0 star1 star2 star3 star4 star5 star6 star7 star8 star9 150 BUTTON2))

(define COLORS (make-colors f1 f2 f3 f4 f5 f6 f7 f8
                             c1 c2 c3 c4 c5 c6 c7 c8 650 BUTTON3))

(define GAME (make-game SHAPES NUMBERS COLORS 0 (make-menu MENU background) (make-gameOver 0 balloon1 balloon2) "menu")) 

(big-bang GAME
  (to-draw drawGame)
  (on-mouse updateGameOnMouse))