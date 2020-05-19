      ******************************************************************
      * This copybook provides the record structure of the report
      * record.
      * It is used to store the data in the VSAM dataset and to map a
      * record payload taken from the database (API subprogram) to
      * generate the report.
      *-----------------------------------------------------------------
      * Author: Michal Blaszak
      * Date:   2020-05-15
      ******************************************************************
           05 FD-RECORD-ID                   PIC X(8)  VALUE ALL SPACES.
           05 FD-DATE                        PIC X(10) VALUE ALL SPACES.
           *> By age
           05 FD-INA-AGE                     PIC 9(7) VALUE ZERO.
           05 FD-LESS-22                     PIC 9(7) VALUE ZERO.
           05 FD-22-24                       PIC 9(7) VALUE ZERO.
           05 FD-25-34                       PIC 9(7) VALUE ZERO.
           05 FD-35-44                       PIC 9(7) VALUE ZERO.
           05 FD-45-54                       PIC 9(7) VALUE ZERO.
           05 FD-55-59                       PIC 9(7) VALUE ZERO.
           05 FD-60-64                       PIC 9(7) VALUE ZERO.
           05 FD-MORE-64                     PIC 9(7) VALUE ZERO.
           *> By gender
           05 FD-INA-GENDER                  PIC 9(7) VALUE ZERO.
           05 FD-FEMALE                      PIC 9(7) VALUE ZERO.
           05 FD-MALE                        PIC 9(7) VALUE ZERO.
           *> By industry
           05 FD-INA-INDUSTRY                PIC 9(7) VALUE ZERO.
           05 FD-WHOLESALE-TRADE             PIC 9(7) VALUE ZERO.
           05 FD-TRANSPORTATION-WAREHOUSE    PIC 9(7) VALUE ZERO.
           05 FD-CONSTRUCTION                PIC 9(7) VALUE ZERO.
           05 FD-FINANCE-INSURANCE           PIC 9(7) VALUE ZERO.
           05 FD-MANUFACTURING               PIC 9(7) VALUE ZERO.
           05 FD-AGRICULT-FORESTRY-FISHING-H PIC 9(7) VALUE ZERO.
           05 FD-PUBLIC-ADMINISTRATION       PIC 9(7) VALUE ZERO.
           05 FD-UTILITIES                   PIC 9(7) VALUE ZERO.
           05 FD-ACCOMODATION-FOOD-SERVICES  PIC 9(7) VALUE ZERO.
           05 FD-INFORMATION                 PIC 9(7) VALUE ZERO.
           05 FD-PROFESSION-SCIENCE-TECH-SER PIC 9(7) VALUE ZERO.
           05 FD-REAL-ESTATE-RENTAL-LEASING  PIC 9(7) VALUE ZERO.
           05 FD-OTHER-SERV-EXCEPT-PUBLIC-AD PIC 9(7) VALUE ZERO.
           05 FD-MGMT-OF-COMPANIES-ENTERPRIS PIC 9(7) VALUE ZERO.
           05 FD-EDUCATIONAL-SERVICES        PIC 9(7) VALUE ZERO.
           05 FD-MINING                      PIC 9(7) VALUE ZERO.
           05 FD-HEALTH-CARE-SOCIAL-ASSISTAN PIC 9(7) VALUE ZERO.
           05 FD-ARTS-ENTERTAINMENT-RECREATI PIC 9(7) VALUE ZERO.
           05 FD-ADMIN-SPRT-WASTE-REMEDIA-SE PIC 9(7) VALUE ZERO.
           05 FD-RETAIL-TRADE                PIC 9(7) VALUE ZERO.
           *> By race
           05 FD-INA-RACE                    PIC 9(7) VALUE ZERO.
           05 FD-WHITE                       PIC 9(7) VALUE ZERO.
           05 FD-ASIAN                       PIC 9(7) VALUE ZERO.
           05 FD-BLACK-OR-AFRICAN-AMERICAN   PIC 9(7) VALUE ZERO.
           05 FD-AMERIC-INDIAN-OR-ALASKA-NAT PIC 9(7) VALUE ZERO.
           05 FD-NATIVE-HAWAII-OR-OTHER-PACI PIC 9(7) VALUE ZERO.
           *> By ethnic
           05 FD-INA-ETHNIC                  PIC 9(7) VALUE ZERO.
           05 FD-HISPANIC-OR-LATINO          PIC 9(7) VALUE ZERO.
           05 FD-NOT-HISPANIC-OR-LATINO      PIC 9(7) VALUE ZERO.
