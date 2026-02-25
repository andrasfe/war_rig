```cobol
              EXEC CICS SEND
                     MAP('COPAU1A')
                     MAPSET('COPAU01')
                     FROM(COPAU1AO)
                     ERASE
                     CURSOR
              END-EXEC
           ELSE
              EXEC CICS SEND
                     MAP('COPAU1A')
                     MAPSET('COPAU01')
                     FROM(COPAU1AO)
                     CURSOR
              END-EXEC
           END-IF
           .

       RECEIVE-AUTHVIEW-SCREEN.

           EXEC CICS RECEIVE
                     MAP('COPAU1A')
                     MAPSET('COPAU01')
                     INTO(COPAU1AI)
                     NOHANDLE
           END-EXEC
           .


       POPULATE-HEADER-INFO.

           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

           MOVE CCDA-TITLE01           TO TITLE01O OF COPAU1AO
           MOVE CCDA-TITLE02           TO TITLE02O OF COPAU1AO
           MOVE WS-CICS-TRANID         TO TRNNAMEO OF COPAU1AO
           MOVE WS-PGM-AUTH-DTL        TO PGMNAMEO OF COPAU1AO

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY

           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF COPAU1AO

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS

           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF COPAU1AO
           .

       READ-AUTH-RECORD.

           PERFORM SCHEDULE-PSB


           MOVE WS-ACCT-ID                TO PA-ACCT-ID
           MOVE WS-AUTH-KEY               TO PA-AUTHORIZATION-KEY

           EXEC DLI GU USING PCB(PAUT-PCB-NUM)
               SEGMENT (PAUTSUM0)
               INTO (PENDING-AUTH-SUMMARY)
               WHERE (ACCNTID = PA-ACCT-ID)
           END-EXEC

           MOVE DIBSTAT                          TO IMS-RETURN-CODE
           EVALUATE TRUE
               WHEN STATUS-OK
                  SET AUTHS-NOT-EOF              TO TRUE
               WHEN SEGMENT-NOT-FOUND
               WHEN END-OF-DB
                  SET AUTHS-EOF                  TO TRUE
               WHEN OTHER
                  MOVE 'Y'     TO WS-ERR-FLG

                  STRING
                  ' System error while reading Auth Summary: Code:'
                  IMS-RETURN-CODE
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  PERFORM SEND-AUTHVIEW-SCREEN
           END-EVALUATE

           IF AUTHS-NOT-EOF
              EXEC DLI GNP USING PCB(PAUT-PCB-NUM)
                  SEGMENT (PAUTDTL1)
                  INTO (PENDING-AUTH-DETAILS)
                  WHERE (PAUT9CTS = PA-AUTHORIZATION-KEY)
              END-EXEC

              MOVE DIBSTAT                          TO IMS-RETURN-CODE
              EVALUATE TRUE
                  WHEN STATUS-OK
                     SET AUTHS-NOT-EOF              TO TRUE
                  WHEN SEGMENT-NOT-FOUND
                  WHEN END-OF-DB
                     SET AUTHS-EOF                  TO TRUE
                  WHEN OTHER
                     MOVE 'Y'     TO WS-ERR-FLG

                     STRING
                     ' System error while reading Auth Details: Code:'
                     IMS-RETURN-CODE
                     DELIMITED BY SIZE
                     INTO WS-MESSAGE
                     END-STRING
                     PERFORM SEND-AUTHVIEW-SCREEN
              END-EVALUATE
           END-IF

           .

       READ-NEXT-AUTH-RECORD.

           EXEC DLI GNP USING PCB(PAUT-PCB-NUM)
               SEGMENT (PAUTDTL1)
               INTO (PENDING-AUTH-DETAILS)
           END-EXEC

           MOVE DIBSTAT                          TO IMS-RETURN-CODE
```
