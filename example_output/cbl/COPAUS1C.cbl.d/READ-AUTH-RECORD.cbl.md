```cobol
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
```
