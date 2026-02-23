             10 CDEMO-CPVD-PAUKEY-LAST     PIC X(08).
             10 CDEMO-CPVD-PAGE-NUM        PIC S9(04) COMP.
             10 CDEMO-CPVD-NEXT-PAGE-FLG   PIC X(01) VALUE 'N'.
                88 NEXT-PAGE-YES                     VALUE 'Y'.
                88 NEXT-PAGE-NO                      VALUE 'N'.
             10 CDEMO-CPVD-AUTH-KEYS       PIC X(08) OCCURS 5 TIMES.
             10 CDEMO-CPVD-FRAUD-DATA      PIC X(100).

       COPY COPAU01.


      *Screen Titles
       COPY COTTL01Y.

      *Current Date
       COPY CSDAT01Y.

      *Common Messages
       COPY CSMSG01Y.

      *Abend Variables
       COPY CSMSG02Y.
