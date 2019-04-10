GLM dual_left dual_right sin1_left sin1_right
  /WSFACTOR=Condition 2 Polynomial Hemifield 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(Condition*Hemifield Condition Hemifield)
  /EMMEANS=TABLES(Condition) COMPARE ADJ(SIDAK)
  /EMMEANS=TABLES(Hemifield) COMPARE ADJ(SIDAK)
  /EMMEANS=TABLES(Condition*Hemifield)  COMPARE(Condition) ADJ(SIDAK)
  /EMMEANS=TABLES(Condition*Hemifield) COMPARE(Hemifield) ADJ(SIDAK)
  /PRINT=DESCRIPTIVE TEST(MMATRIX)  ETASQ
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=Condition Hemifield Condition*Hemifield.