# UT_FVS
Using tree-ring and forest inventory data to create a species-specific tree growth model to inform a widely used forest management tool - the Forest Vegetation Simulator.

## Project Summary

Globally, forests remove about a third of anthropogenic carbon emissions from the atmosphere (Le Quéré et al. 2015). However, projections of future forest uptake of carbon dioxide are uncertain in the face of changing climate (Friedlingstein et al. 2014). Forestry growth and yield models, such as the Forest Vegetation Simulator (FVS), have been used to better understand forests’ response to changing climate. The widely used FVS inputs tree and plot data and simulates growth, mortality, and response to treatments for forest stands to inform management decisions. Although a recent version of FVS modifies forest development based on a species’ estimated climatic niche, the current version of FVS lacks the direct effect of climate variation on diameter growth. Through the recent development of a tree-ring data network throughout the interior West by the U.S. Forest Service’s Interior West Forest Inventory and Analysis (IW-FIA) program (DeRose, Shaw, and Long 2017), a unique opportunity is available to use the rich information in tree-rings, complemented by inventory data to parameterize FVS growth models. I am using the climate response recorded in tree-rings, combined with monitoring data, to parameterize the tree growth models for several tree species in the Utah variant of FVS. In doing so, I am developing a method for incorporating tree-rings into FVS, with the Utah variant as a testing ground. While tree rings have long been a tool that foresters use to assess tree growth, the climate fingerprint recorded in tree rings has never been incorporated into stand-level growth and yield models. Ring width fluctuates with year-to-year variation in the climate and site conditions during the growing season. Combining tree rings and forest inventory data allows me to parse the multiple drivers of tree growth and forest stand development, such as climate, competition, and site characteristics. We expect the inclusion of climate in the diameter growth models will provide more accurate projections of carbon uptake, allowing foresters to anticipate stand vulnerability to climate change and adaptively manage to increase stand resilience.

## Table of Contents

1. /scripts - code to make a tree ring and forest inventory data set, build growth models, and assess models.
2. /Rmarkdown - markdown files for each species and validation process.
3. /images - images for the wiki and from outputs.

## Usage

For more information, see project [Wiki](https://github.com/clgiebink/UT_FVS/wiki). 
