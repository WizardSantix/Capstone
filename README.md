# Capstone
Capstone project for the data science specialization coursera.

# About the project
The goal of this exercise is to create a product to highlight the prediction algorithm that you have built and to provide an interface that can be accessed by others. For this project you must submit:
- A Shiny app that takes as input a phrase (multiple words) in a text box input and outputs a prediction of the next word.
- A slide deck consisting of no more than 5 slides created with R Studio Presenter (https://support.rstudio.com/hc/en-us/articles/200486468-Authoring-R-Presentations) pitching your algorithm and app as if you were presenting to your boss or an investor.

## File Index
- https://github.com/WizardSantix/Capstone/blob/master/Report(w2).Rmd Contains the Rmd for the milestone report of week 2, detailing the preparatory analysis.
- https://github.com/WizardSantix/Capstone/blob/master/Create%20datasets.R This piece of code creates datasets of n-grams that are used by the app as input.
- https://github.com/WizardSantix/Capstone/blob/master/Calculate%20prob%20and%20conditional%20prob.R Script for the prediction model that was used in the app.
- https://github.com/WizardSantix/Capstone/tree/master/Word_predictor contains the server and UI that compose the app, note that for these to work .feather datasets must be created using the code above.
