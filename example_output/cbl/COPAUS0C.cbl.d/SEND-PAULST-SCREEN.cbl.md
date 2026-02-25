```cobol
           EVALUATE TRUE
               WHEN STATUS-OK
                  SET FOUND-PAUT-SMRY-SEG        TO TRUE
               WHEN SEGMENT-NOT-FOUND
                  SET NFOUND-PAUT-SMRY-SEG       TO TRUE
               WHEN OTHER
                  MOVE 'Y'     TO WS-ERR-FLG

                  STRING
                  ' System error while reading AUTH Summary: Code:'
                  IMS-RETURN-CODE
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
           END-EVALUATE
           .
      *****************************************************************
      * SCHEDULE PSB                                                  *
      *****************************************************************
       SCHEDULE-PSB.
           EXEC DLI SCHD
                PSB((PSB-NAME))
                NODHABEND
           END-EXEC
           MOVE DIBSTAT        TO IMS-RETURN-CODE
           IF PSB-SCHEDULED-MORE-THAN-ONCE
              EXEC DLI TERM
              END-EXEC

              EXEC DLI SCHD
                   PSB((PSB-NAME))
                   NODHABEND
              END-EXEC
              MOVE DIBSTAT     TO IMS-RETURN-CODE
           END-IF
           IF STATUS-OK
              SET IMS-PSB-SCHD           TO TRUE
           ELSE
              MOVE 'Y'     TO WS-ERR-FLG
```
