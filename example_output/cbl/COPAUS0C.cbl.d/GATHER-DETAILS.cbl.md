```cobol


      *****************************************************************
       GET-AUTHORIZATIONS.
      *****************************************************************

           EXEC DLI GNP USING PCB(PAUT-PCB-NUM)
               SEGMENT (PAUTDTL1)
               INTO (PENDING-AUTH-DETAILS)
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

```
