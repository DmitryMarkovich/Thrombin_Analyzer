You are successfully logged in the [Thrombin
Analyzer](https://dmitriymarkovich.shinyapps.io/Thrombin_Analyzer/) application!
Press `Log out` in the topright corner of the sidebar to log out of the
application.

This application performs data analysis in thrombin generation
experiments. Below is the step-by-step guide that explains how to use it.

> #### Calibration

**Calibration signal** tab visualizes the results of analysis of a calibration
signal.  Use the `Load calibration data file` button and choose a suitable
signal to load. Then, choose a model to fit the loaded signal using the `Select
model to fit calibration signal`. Parameters obtained from the model fitted to
the signal will be shown in **Parameters** tab.

> #### Thrombin generation

**Thrombin generation signal** tab visualizes the results of analysis of a
thrombin generation signal.  Use the `Load thrombin generation data file` button
and choose a suitable signal to load. Then, choose a model to fit the loaded
signal using the `Select model to fit thrombin generation signal`. Use
**Thrombogram** tab to visualize the thrombogram and the thrombin velocity of
the signal. Parameters obtained from the model fitted to the signal will be
shown in **Parameters** tab.

> #### Parameters

Analyze a calibration or a thrombin generation signal or both following the
steps above and check the parameters with the **Parameters** tab.

> #### Controls

* **SC time, min** the minimal length of the measurement that will cause the app
  to try out models with substrate consumption (SC).  If You want all the
  signals in Your dataset to use models without SC, set it to a value larger
  than the maximal time in the dataset.  The default value is 121 --- all
  signals that are shorter than 121 minutes will not be fitted with SC models by
  the **Auto** model.
* **SC ratio** this ratio is used by **Auto** model to make a decision whether
  SC model should be preferred to the non-SC model.  If the residual standard
  error of the fit without SC is greater than **SC ratio** multiplied by the
  residual standard error of the fit with SC, than SC model would not be
  preferred.  It means that if a fit of an SC model does not increase the
  residual standard error of the fit enough, there is no need for SC model for
  this signal.  The default value is 1.7.
* **Mode** --- this radio button switches the mode of the app from
  **Speed** to **Accuracy**.  In **Speed** mode, no attempts to fit
  more complex models will be made to save time, whereas in **Accuracy** mode
  the app will persistently try to fit more complicated models to the data for
  better approximation.  The default mode is **Speed**.

> #### Dataset

**Dataset** tab visualizes the results of the analysis of a dataset from a
thrombin generation experiment. Use the `Load dataset file` to load a dataset,
and press `Analyze!` to analyze it.  Overlay thrombograms and check the details
of the performed analysis using the **Overlay** and **Overlay details** tabs.

If You have the results and the parameters available for the dataset, You can
alternatively load them using the `Load parameters` and `Load results`.

> #### Demo signals

In case there are no data on Your computer, download some of the provided demo
signals by choosing from options in `Select a demo signal to try out models on
data!`. The data You choose is visualized in **Demo signals** tab. Download
data, and analyze it following the steps above.

> #### Tutorial

Shows Thrombin Analyzer Tutorial directly in the app using the standard
pdf-viewer of Your browser.

Good luck!

[Dmitriy Markovich](http://www.nanotech.dtu.dk/english/Service/Phonebook/Person?id=69192&tab=0)
