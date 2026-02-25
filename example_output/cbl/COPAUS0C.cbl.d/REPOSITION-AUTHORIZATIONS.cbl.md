```cobol
                   MOVE SPACES   TO PTYPE04I OF COPAU0AI
                   MOVE SPACES   TO PAPRV04I OF COPAU0AI
                   MOVE SPACES   TO PSTAT04I OF COPAU0AI
                   MOVE SPACES   TO PAMT004I OF COPAU0AI
               WHEN 5
                   MOVE DFHBMPRO TO SEL0005A OF COPAU0AI
                   MOVE SPACES   TO TRNID05I OF COPAU0AI
                   MOVE SPACES   TO PDATE05I OF COPAU0AI
                   MOVE SPACES   TO PTIME05I OF COPAU0AI
                   MOVE SPACES   TO PTYPE05I OF COPAU0AI
                   MOVE SPACES   TO PAPRV05I OF COPAU0AI
                   MOVE SPACES   TO PSTAT05I OF COPAU0AI
                   MOVE SPACES   TO PAMT005I OF COPAU0AI
               WHEN OTHER
                   CONTINUE
             END-EVALUATE
           END-PERFORM
           .

      *****************************************************************
       RETURN-TO-PREV-SCREEN.
      *****************************************************************

           IF CDEMO-TO-PROGRAM = LOW-VALUES OR SPACES
               MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
           END-IF
           MOVE WS-CICS-TRANID  TO CDEMO-FROM-TRANID
           MOVE WS-PGM-AUTH-SMRY TO CDEMO-FROM-PROGRAM
           MOVE ZEROS           TO CDEMO-PGM-CONTEXT
           EXEC CICS
               XCTL PROGRAM(CDEMO-TO-PROGRAM)
               COMMAREA(CARDDEMO-COMMAREA)
           END-EXEC.


      *****************************************************************
       SEND-PAULST-SCREEN.
      *****************************************************************

           IF IMS-PSB-SCHD
              SET IMS-PSB-NOT-SCHD      TO TRUE
```
