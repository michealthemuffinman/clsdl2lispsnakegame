;;; snakeatt.lisp
;;; A simple Snake game implemented in Common Lisp using SDL2

(require :sdl2)

;; Utility Functions

(defun clamp (value min max)
  "Clamp the value to be between min and max."
  (max min (min value max)))

(defun random-color ()
  "Generate a random color in the form of (R G B)."
  (list (random 256) (random 256) (random 256)))

(defun draw-rect (renderer x y width height color)
  "Draw a rectangle with the specified color."
  (destructuring-bind (r g b) color
    (sdl2:set-render-draw-color renderer r g b 255))  ;; Set color with alpha 255
  (let ((rect (sdl2:make-rect (truncate x)
                              (truncate y)
                              (truncate width)
                              (truncate height))))
    (sdl2:render-fill-rect renderer rect)))  ;; Draw the filled rectangle

(defun rect-collides-p (x1 y1 w1 h1 x2 y2 w2 h2)
  "Check if two rectangles collide."
  (not (or (>= x1 (+ x2 w2))
           (>= x2 (+ x1 w1))
           (>= y1 (+ y2 h2))
           (>= y2 (+ y1 h1)))))

;; Game Constants

(defparameter *window-width* 800)
(defparameter *window-height* 600)
(defparameter *grid-size* 20)  ;; Size of each grid block in pixels
(defparameter *initial-snake-length* 5)
(defparameter *snake-speed* 10)  ;; Moves per second
(defparameter *food-color* '(255 0 0))  ;; Red
(defparameter *snake-color* '(0 255 0))  ;; Green
(defparameter *background-color* '(0 0 0))  ;; Black

;; Helper Functions

(defun random-grid-position ()
  "Generate a random position on the grid."
  (list
   (* (random (truncate (/ *window-width* *grid-size*))) *grid-size*)
   (* (random (truncate (/ *window-height* *grid-size*))) *grid-size*)))

(defun positions-equal-p (pos1 pos2)
  "Check if two positions are equal."
  (and (= (first pos1) (first pos2))
       (= (second pos1) (second pos2))))

;; Game State

(defstruct game-state
  snake           ;; List of positions (head first)
  direction       ;; Current direction (:up, :down, :left, :right)
  food            ;; Position of the food
  alive)          ;; Boolean indicating if the game is running

;; Define RANDOM-FOOD-POSITION before INITIALIZE-GAME

(defun random-food-position (snake)
  "Generate a random food position not occupied by the snake."
  (let ((pos (random-grid-position)))
    (if (find pos snake :test #'positions-equal-p)
        (random-food-position snake)  ;; Retry if position is on the snake
        pos)))

(defun initialize-game ()
  "Initialize the game state."
  (let* ((start-x (* 10 *grid-size*))  ;; Starting position
         (start-y (* 15 *grid-size*))
         (initial-snake (loop for i from 0 below *initial-snake-length*
                             collect (list start-x (- start-y (* i *grid-size*))))))
    (make-game-state
     :snake initial-snake
     :direction :right
     :food (random-food-position initial-snake)
     :alive t)))

;; Game Logic

(defun change-direction (current new)
  "Change direction ensuring the snake cannot reverse."
  (cond
    ((and (eq current :up) (eq new :down)) current)
    ((and (eq current :down) (eq new :up)) current)
    ((and (eq current :left) (eq new :right)) current)
    ((and (eq current :right) (eq new :left)) current)
    (t new)))

(defun update-snake (state)
  "Update the snake's position based on the current direction."
  (let* ((snake (game-state-snake state))
         (head (car snake))
         (new-head (case (game-state-direction state)
                     (:up (list (first head) (- (second head) *grid-size*)))
                     (:down (list (first head) (+ (second head) *grid-size*)))
                     (:left (list (- (first head) *grid-size*) (second head)))
                     (:right (list (+ (first head) *grid-size*) (second head))))))
    ;; Add new head
    (setf (game-state-snake state)
          (cons new-head (butlast snake)))))  ;; Remove tail

(defun grow-snake (state)
  "Grow the snake by adding a new head without removing the tail."
  (let* ((snake (game-state-snake state))
         (head (car snake))
         (new-head (case (game-state-direction state)
                     (:up (list (first head) (- (second head) *grid-size*)))
                     (:down (list (first head) (+ (second head) *grid-size*)))
                     (:left (list (- (first head) *grid-size*) (second head)))
                     (:right (list (+ (first head) *grid-size*) (second head))))))
    ;; Add new head without removing tail
    (setf (game-state-snake state)
          (cons new-head snake))))

(defun check-collisions (state)
  "Check for collisions with walls or self."
  (let* ((snake (game-state-snake state))
         (head (car snake))
         (x (first head))
         (y (second head))
         (body (cdr snake)))
    (when (or (< x 0)
              (>= x *window-width*)
              (< y 0)
              (>= y *window-height*)
              (find head body :test #'positions-equal-p))
      (setf (game-state-alive state) nil)
      ;; Push a :quit event to exit the event loop
      (sdl2:push-event :quit))))

(defun check-food (state)
  "Check if the snake has eaten the food."
  (when (positions-equal-p (car (game-state-snake state)) (game-state-food state))
    (grow-snake state)
    (setf (game-state-food state)
          (random-food-position (game-state-snake state)))))

(defun update-game (state)
  "Update the game state."
  (when (game-state-alive state)
    (update-snake state)
    (check-collisions state)
    (check-food state)
    ;; Optional Debugging Output
    ;; (format t "Snake Head: ~a~%" (car (game-state-snake state)))
    ))

;; Rendering

(defun render-game (renderer state)
  "Render the current game state."
  ;; Clear the screen
  (destructuring-bind (r g b) *background-color*
    (sdl2:set-render-draw-color renderer r g b 255))
  (sdl2:render-clear renderer)

  ;; Draw the food
  (destructuring-bind (fx fy) (game-state-food state)
    (draw-rect renderer fx fy *grid-size* *grid-size* *food-color*))

  ;; Draw the snake
  (dolist (segment (game-state-snake state))
    (destructuring-bind (x y) segment
      (draw-rect renderer x y *grid-size* *grid-size* *snake-color*)))

  ;; Present the rendered frame
  (sdl2:render-present renderer))

;; Main Game Loop

(defun snake-game ()
  "Run the Snake game."
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Snake Game"
                            :w *window-width*
                            :h *window-height*
                            :flags '(:shown))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
        (let ((state (initialize-game))
              (keys-pressed (make-hash-table :test 'eq))
              (last-time (sdl2:get-ticks))
              (move-interval (/ 1000.0 *snake-speed*)))  ;; Move interval in milliseconds

          (sdl2:with-event-loop (:method :poll)
            ;; Define event handlers
            (:keydown (:keysym keysym)
                      (let ((sym (sdl2:scancode-value keysym)))
                        (cond
                          ;; Use scancode labels for WASD
                          ((sdl2:scancode= sym :scancode-w) (setf (gethash :up keys-pressed) t))
                          ((sdl2:scancode= sym :scancode-s) (setf (gethash :down keys-pressed) t))
                          ((sdl2:scancode= sym :scancode-a) (setf (gethash :left keys-pressed) t))
                          ((sdl2:scancode= sym :scancode-d) (setf (gethash :right keys-pressed) t))
                          ((sdl2:scancode= sym :scancode-escape) (sdl2:push-event :quit)))))

            (:keyup (:keysym keysym)
                    (let ((sym (sdl2:scancode-value keysym)))
                      (cond
                        ;; Use scancode labels for WASD
                        ((sdl2:scancode= sym :scancode-w) (remhash :up keys-pressed))
                        ((sdl2:scancode= sym :scancode-s) (remhash :down keys-pressed))
                        ((sdl2:scancode= sym :scancode-a) (remhash :left keys-pressed))
                        ((sdl2:scancode= sym :scancode-d) (remhash :right keys-pressed))
                        ((sdl2:scancode= sym :scancode-escape) (sdl2:push-event :quit)))))

            (:quit () t)  ;; Exit the loop when a :quit event is received

            (:idle ()
                   (let ((current-time (sdl2:get-ticks))
                         (dt 0))
                     (setf dt (- current-time last-time))
                     (when (>= dt move-interval)  ;; Time to move the snake
                       ;; Determine new direction based on keys pressed
                       (let ((new-direction
                              (cond
                                ((gethash :up keys-pressed) :up)
                                ((gethash :down keys-pressed) :down)
                                ((gethash :left keys-pressed) :left)
                                ((gethash :right keys-pressed) :right)
                                (t (game-state-direction state)))))
                         (setf (game-state-direction state)
                               (change-direction (game-state-direction state) new-direction))
                         (update-game state)
                         (setf last-time current-time)))

                     ;; Render the game
                     (render-game renderer state)

                     ;; Delay to cap frame rate (~60 FPS)
                     (sdl2:delay 16))))

          ;; After exiting the event loop, check if the game was over
          (when (not (game-state-alive state))
            (format t "Game Over!~%")))))))

;; Start the game
(snake-game)

