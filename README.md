# stp25aggregate - Data and Variable Transformation Functions 


Long/Wide
---------

Umformen von einem Breit-Format nach einem Lang-Format. Melt2 und melt2
sind Erweiterungen der reshape2::melt Funktion. Intern wird *melt* und
*dcast* verwendet.

    #>   month student A B
    #> 1     1     Amy 9 6
    #> 2     2     Amy 7 7
    #> 3     3     Amy 6 8
    #> 4     1     Bob 8 5
    #> 5     2     Bob 6 6
    #> 6     3     Bob 9 7

### Vergleich der Ergebnisse spread vs Wide

    tidyr::spread(df[-4], student, A)
    #>   month Amy Bob
    #> 1     1   9   8
    #> 2     2   7   6
    #> 3     3   6   9
    (df_w1 <- Wide(df[-4], student, A))
    #>   month Amy Bob
    #> 1     1   9   8
    #> 2     2   7   6
    #> 3     3   6   9

### Vergleich der Ergebnisse gather vs Long

     
     tidyr::gather(df_w1,  key = "student", value = "A", Amy, Bob)   
    #>   month student A
    #> 1     1     Amy 9
    #> 2     2     Amy 7
    #> 3     3     Amy 6
    #> 4     1     Bob 8
    #> 5     2     Bob 6
    #> 6     3     Bob 9

     Long(Amy + Bob ~ month, df_w1, key="student", value="A") 
    #>   month student A
    #> 1     1     Amy 9
    #> 2     2     Amy 7
    #> 3     3     Amy 6
    #> 4     1     Bob 8
    #> 5     2     Bob 6
    #> 6     3     Bob 9

     Long(df_w1, id.vars=1, key = "student", value = "A")
    #>   month student A
    #> 1     1     Amy 9
    #> 2     2     Amy 7
    #> 3     3     Amy 6
    #> 4     1     Bob 8
    #> 5     2     Bob 6
    #> 6     3     Bob 9

### Long mit meheren Parametern


    (df_w2 <- Wide(df, student, c(A, B)))
    #>   month Amy_A Amy_B Bob_A Bob_B
    #> 1     1     9     6     8     5
    #> 2     2     7     7     6     6
    #> 3     3     6     8     9     7
     Long(list(A=c("Amy_A", "Bob_A" ), B=c("Amy_B", "Bob_B")), df_w2,
                 by =  ~ month,
                 key = "student",
                 key.levels= c("Amy", "Bob"))
    #>   month student A B
    #> 1     1     Amy 9 6
    #> 2     2     Amy 7 7
    #> 3     3     Amy 6 8
    #> 4     1     Bob 8 5
    #> 5     2     Bob 6 6
    #> 6     3     Bob 9 7

### Berechnen

    Summarise(A + B ~ student, df, mean3, key = "group", value = "cbc")
    #>   student group cbc
    #> 1     Amy     A 7.3
    #> 2     Amy     B 7.0
    #> 3     Bob     A 7.7
    #> 4     Bob     B 6.0


    Summarise(A + B ~ student, df, mean3,  margins = TRUE)
    #>    student variable value
    #> 1      Amy        A   7.3
    #> 2      Amy        B   7.0
    #> 3      Bob        A   7.7
    #> 4      Bob        B   6.0
    #> 11  gesamt        A   7.5
    #> 21  gesamt        B   6.5

    Summarise(A + B ~ student,
                      df,
                      mean3,
                      formula = variable ~ student,
                      margins = TRUE)
    #>   variable Amy Bob (all)
    #> 1        A 7.3 7.7   7.5
    #> 2        B 7.0 6.0   6.5
    #> 3    (all) 7.2 6.8   7.0

### Aufdröseln vom Mehrfachantworten

Die Funktion separate\_multiple\_choice() transformiert einen String mit
Trennzeichen zu einem Multi-Set mit 0 und 1. (Separate multiple choice)
der param x ist entweder ein Character oder eine zahl

    dat <-  data.frame(
      Q1 = c(134, NA, 35, 134, 5, 24),
      Q2 = c(
        "Alm Dudler, Essig, Cola",  NA, 
        "Cola, Holer", "Alm Dudler, Cola, 
        Essig","Holer", "Bier, Essig"
      )
    )
    dat
    #>    Q1                            Q2
    #> 1 134       Alm Dudler, Essig, Cola
    #> 2  NA                          <NA>
    #> 3  35                   Cola, Holer
    #> 4 134 Alm Dudler, Cola, \n    Essig
    #> 5   5                         Holer
    #> 6  24                   Bier, Essig


    cbind(dat[-1],
          separate_multiple_choice(dat$Q2))
    #> Warning: Expected 7 pieces. Missing pieces filled with `NA` in 6 rows [1,
    #> 2, 3, 4, 5, 6].
    #>                              Q2 Q2_1 Q2_2 Q2_3 Q2_4 Q2_5 Q2_6
    #> 1       Alm Dudler, Essig, Cola nein   ja nein   ja   ja nein
    #> 2                          <NA> <NA> <NA> <NA> <NA> <NA> <NA>
    #> 3                   Cola, Holer nein nein nein   ja nein   ja
    #> 4 Alm Dudler, Cola, \n    Essig   ja   ja nein   ja nein nein
    #> 5                         Holer nein nein nein nein nein   ja
    #> 6                   Bier, Essig nein nein   ja nein   ja nein


    dat <- cbind(dat[-2],
                 separate_multiple_choice(dat$Q1,
                                          label = c(
                                            "Alm Dudler", "Bier", "Cola", "Essig", "Holer"
                                          )))
    #> Warning: Expected 6 pieces. Missing pieces filled with `NA` in 6 rows [1,
    #> 2, 3, 4, 5, 6].

    names(dat) <- GetLabelOrName(dat)
    dat
    #>    Q1 Q1_1 Q1_2 Q1_3 Q1_4 Q1_5
    #> 1 134   ja nein   ja   ja nein
    #> 2  NA <NA> <NA> <NA> <NA> <NA>
    #> 3  35 nein nein   ja nein   ja
    #> 4 134   ja nein   ja   ja nein
    #> 5   5 nein nein nein nein   ja
    #> 6  24 nein   ja nein   ja nein
