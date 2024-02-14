function(){
  tabPanel(HTML('<span style="font-size:100%;color:white;font-weight:bold;">About</span></a>'),
           HTML('
      <div style="display:inline;float:right;margin:0px 0px 5px 20px">
        <img src="logo.png" border="0" width="100" style="margin:0px">
      </div>

      <div style="max-width:1000px; word-wrap:break-word;">
        <p style="font-size:120%;text-align:justify">
          This interactive web application has been designed to provide an easy-to-use interface to visualize the 
          gender-, age- and educational-based mismatch in hourly population profiles at district level in France.
          It was developed in the framework of the <a href="https://www.mobiliscope.cnrs.fr" target=_blank>Mobiliscope</a>.
        </p>
      </div>   

      <br>
      
      <div style="max-width:1000px; word-wrap:break-word;">
        <p style="font-size:120%;text-align:justify">
          This interactive application is composed of three tabs that focus on the visualisation of several analysis 
          conducted in <a href="https://journals.sagepub.com/doi/abs/10.1177/23998083231174025" target=_blank>this paper [1]</a>. The first tab (<span style="color:#64645F;font-weight:bold;">Profiles</span>) presents the spatial distribution
          of the five hourly profiles according to the total population and social groups according to
          gender (men and women), age groups (16-24 years, 25-34 years, 35-64 years and 65 years and more) 
          and levels of education (low, middle-low, middle-high and high). The second tab (<span style="color:#64645F;font-weight:bold;">Mismatch</span>) displays a map of 
          mismatches in hourly profiles within gender, age and education social groups. Finally, the third tab (<span style="color:#64645F;font-weight:bold;">Cluster</span>)
          presents the spatial distribution of districts exhibiting similarities in their within-mismatch values 
          as resulting from cluster analysis.
        </p>
      </div> 

      <br>

      <div style="max-width:1000px; word-wrap:break-word;">
        <p style="font-size:120%;text-align:justify;">
           Initial data of hourly populations estimations in every district (coming from origin-destination surveys) are displayed in 
           the <a href="https://www.mobiliscope.cnrs.fr" target=_blank>Mobiliscope</a> platform (v4.1) and made 
           also available under ODbL license <a href="https://doi.org/10.5281/zenodo.7738571" target=_blank>here [2]</a>.
           We thank the Mobiliscope team (in particular Aurélie Douet and Guillaume Le Roux), its partners and funders 
           (CNRS, ANCT, Ined and Labex DynamiTe) as well as the French data producers (Cerema and DRIEA) and their main 
           distributor (Progedo-Adisp). We thank also members of the collective Eighties for the stimulating exchanges.</p>
      </div>  

      <hr width="1000", align="left" style="height:0.5px;border:none;color:#A0A5A8;background-color:#A0A5A8;" />

  
		  		  <span style="color:#64645F;font-weight: bold;">Contributors</span>
		  <div style="max-width:1000px; word-wrap:break-word;">
		     <p style="text-align:justify">
		        <a href="http://www.maximelenormand.com" target=_blank>Maxime Lenormand</a> & <a href="https://geographie-cites.cnrs.fr/en/members/julie-vallee/" target=_blank>Julie Vall&#233;e</a> 
		     </p>
		  </div>  

      <span style="color:#64645F;font-weight:bold;">References</span>
		  <div style="max-width:1000px; word-wrap:break-word;">
		     <p style="text-align:justify;">
		     
		     [1] Vall&#233;e J & Lenormand M (2024) <a href="https://journals.sagepub.com/doi/abs/10.1177/23998083231174025" target=_blank>Intersectional approach of everyday geography.</a>  <i>Environment and Planning B: Urban Analytics and City Science</i> 51, 346-365.
		     <br> <a href="https://doi.org/10.1177/23998083231174025" target=_blank>https://doi.org/10.1177/23998083231174025</a> <br>
		     [2] Vall&#233;e J, Douet A, Le Roux G, Commenges H, Lecomte C & Villard E (2022) <a href="https://doi.org/10.5281/zenodo.7738571" target=_blank>Mobiliscope, a geovisualization platform to explore cities around the clock (v4.1_Extract of French Data) [Data set].</a> <i>Zenodo</i> <a href="https://doi.org/10.5281/zenodo.7738571" target=_blank>https://doi.org/10.5281/zenodo.7738571</a> 
		     
		     </p>
		  </div>  
		  
		  <span style="color:#64645F;font-weight:bold;">Code & Data</span>
		  <div style="max-width:1000px; word-wrap:break-word;">
		     <p style="text-align:justify;">
		        Source code available <a href="https://gitlab.huma-num.fr/mobiliscope/intersectionality" target=_blank>here</a>
		     </p>
		  </div> 

		  <span style="color:#64645F;font-weight:bold;">License</span>
		  <div style="max-width:1000px; word-wrap:break-word;">
		     <p style="text-align:justify;">
		        Coded under License GPLv3
		     </p>
		  </div> 
		'),
           value = "about"
  )
}
