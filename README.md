# ConnectX

## Known Bugs

1. Game terminating late

    1. Compile
    2. ./ConnectX -hB
    3. Win the first round (when blue goes first)
    4. Enter moves: 2, 3, 4
    5. At this point red should have won. But the game still prompts for blue to enter a move.
    
        ```
        Enter move for BlueBot (1-8):
        4
        >
        1 | 2 | 3 | 4 | 5 | 6 | 7 | 8
        -----------------------------
          |   |   |   |   |   |   |  
          |   |   |   |   |   |   |  
          |   |   |   |   |   |   |  
          |   |   |   |   |   |   |  
          |   |   |   |   |   |   |  
        R |   |   |   |   |   |   |  
        R |   |   |   |   |   |   |  
        R | B | B | B |   |   |   |  
        -----------------------------
        1 | 2 | 3 | 4 | 5 | 6 | 7 | 8
        -
        0.000-
        0.000-
        0.000>
        1 | 2 | 3 | 4 | 5 | 6 | 7 | 8
        -----------------------------
          |   |   |   |   |   |   |  
          |   |   |   |   |   |   |  
          |   |   |   |   |   |   |  
          |   |   |   |   |   |   |  
        R |   |   |   |   |   |   |  
        R |   |   |   |   |   |   |  
        R |   |   |   |   |   |   |  
        R | B | B | B |   |   |   |  
        -----------------------------
        1 | 2 | 3 | 4 | 5 | 6 | 7 | 8
        Enter move for BlueBot (1-8):

        
        ```