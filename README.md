# engcalc
Engineers typically need to show working out as part of the solution process for quality assurance and checking. 
`engcalc` uses the symbolic manipulation capability of the `Ryacas` package to solve engineering calculations
such as those found in structural engineering or other technical disciplines and it enables the user to also see the
substituted, but not solved equation so that the origin of each variable is traceable and the solution can be validated and verified.

In this manner, the input variables, the equations used, their substituted form, and the solution are avaialble to the user. The 
package outputs both the `R` and `LaTeX` form of the formulae so that they can be used in further calculations as well as shown
in an engineering report:

TODO: 
1. Integrate the `units` package for dimensionally consistent calculations
2. Develop a `LaTeX` calculation template for typesetting of calculations
3. Develop HTML style for interactive calulations
4. Integrate with `learnr` so that `engcalc` can be used for teaching
5. Better integration and differentiation support
