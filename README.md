# Excellence in Agronomy - Carob

This contains the *carob* compliant scripts to standardize Excellence in Agronomy (EiA) CGIAR initiative data. This is a separate repository in order to avoid risks of conflicts between the 2 repositories. For now, the datasets are not public, but once they are, they will be added to the main carob project.

For this activity, we are using EiA specific URIs. We rely on DELIVER use case descriptions to categorize the different use cases and their activities. This information can be obtained from [here](https://my.eia.cgiar.org/api/v1/usecases/). Additionally, certain adjustments need to be carried in the [`terminag`](https://github.com/reagro/terminag), such as include a new group (`eia`) and metadata variables and values. These are included on the [`terms`](https://github.com/EiA2030/eia-carob) folder of this repository. You will need to add these to the relevant installation of the `carobiner` terms.

For additional information on what is carob and how it is used, please visit [https://github.com/reagro/carob](https://github.com/reagro/carob) or the [carob-data website](https://carob-data.org/).
