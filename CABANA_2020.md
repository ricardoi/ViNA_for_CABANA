---


---

<h1 id="cabana-workshop-san-jose-costa-rica--university-of-costa-rica">CABANA Workshop San Jose Costa Rica @ University of Costa Rica</h1>
<h3 id="virome-network-analysis--v-i-n-a-framework-adapoted-for-virusdetect">Virome Network Analysis  (V <em>i</em> N A) framework adapoted for VirusDetect</h3>
<h4 id="code-developed-by-ricardo-i-alcala-ph.d.-candidate--garrett-lab-university-of-florida">Code developed by Ricardo I Alcala Ph.D. candidate | @Garrett lab University of Florida</h4>
<h5 id="contact-ralcalaufl.edu">contact: <a href="mailto:ralcala@ufl.edu">ralcala@ufl.edu</a></h5>
<hr>
<pre class=" language-r"><code class="prism  language-r"><span class="token comment"># R version 3.6.2</span>

<span class="token comment">#@------------------------ SET UP ENVIRONMENT ------------------------</span>
<span class="token comment">#@------ packages #uncomment to install -----</span>
<span class="token comment"># Install Libraries:</span>
<span class="token comment"># install.packages("tidyr")</span>
<span class="token comment"># install.packages("igraph")</span>
<span class="token comment"># install.packages("bipartite")</span>
<span class="token comment"># install.packages("XML")</span>
<span class="token comment"># install.packages("htmltab")</span>
<span class="token comment">#@ load libraries</span>
library<span class="token punctuation">(</span>plyr<span class="token punctuation">)</span>
library<span class="token punctuation">(</span>tidyverse<span class="token punctuation">)</span>
library<span class="token punctuation">(</span>tidyr<span class="token punctuation">)</span>
library<span class="token punctuation">(</span>igraph<span class="token punctuation">)</span>
library<span class="token punctuation">(</span>bipartite<span class="token punctuation">)</span>
library<span class="token punctuation">(</span>XML<span class="token punctuation">)</span>
library<span class="token punctuation">(</span>htmltab<span class="token punctuation">)</span>

</code></pre>
<h2 id="notas">Notas:</h2>
<p>Jan no confio cuando hay minimo dos mistmatches en los siRNAs</p>
<p>Los 21 y 22 nt son mayoritariamente de virus<br>
Los de 24 son mayoritariamente de origen vegetal</p>
<p>En```R<br>
#@---- Parsing function -----<br>
dat.parse &lt;- function(df, location, host){</p>
<pre><code>```R
# Formatting metadata
df$sampleID &lt;- location
df$host &lt;- host
# ReGex and residuals removal
names &lt;- str_extract(df$Description, ".*virus[:space:][:alnum:]")
a &lt;- gsub("virus i", "virus", names)
a &lt;- gsub("virus s", "virus", a)
# if there are more 'CHARACTER' after virus, add another line as follows
# a &lt;- gsub("virus s", "virus", 'CHARACTER')
df$Species &lt;- gsub("virus p", "virus", a)
# Generating pseudo-acronyms
df$acronym &lt;- abbreviate(df$Species)
print(df)
}
</code></pre>
<p>#----- setwd -----<br>
setwd("…/ViNA_for_CABANA-master/")<br>
#------------------------ DATA MANIPULATION ------------------------<br>
#---- Loading data from Virus Detect -----<br>
loaded_files &lt;- list(<br>
vd.re1 &lt;- readHTMLTable(‘Cca59_blastn.html’),<br>
vd.re2 &lt;- readHTMLTable(‘Czo24_blastn.html’),<br>
vd.re3 &lt;- readHTMLTable(‘Czo25_blastn.html’),<br>
vd.re4 &lt;- readHTMLTable(‘Czo42_blastn.html’),<br>
vd.re5 &lt;- readHTMLTable(‘Czo56_blastn.html’),<br>
vd.re6 &lt;- readHTMLTable(‘Hua25_blastn.html’),<br>
vd.re7 &lt;- readHTMLTable(‘Ica87_blastn.html’),<br>
vd.re8 &lt;- readHTMLTable(‘Jin116_blastn.html’)<br>
)</p>
<pre><code>&gt; Note:
#loaded_files[[1]] # change this number to check other inputs [['n']]
</code></pre>
<pre><code>#@---- Formatting as tibble and numbers -----
results.ls = temp = list(NULL)
for (k in seq_along(loaded_files)){
	temp[[k]] &lt;- as_tibble(loaded_files[[k]]$`NULL`[1:dim(loaded_files[[k]]$`NULL`)[2]])
	results.ls[[k]] &lt;- temp[[k]] %&gt;%
		mutate(`Depth (Norm)` = as.numeric(as.character(`Depth (Norm)`)) )
}
results.ls
</code></pre>
<pre><code>#@--- Adding metadata to Virus Detect cuando no se parecen a nada, se van a un 'archivo' especial de no 'matches' que 
pueden tener secuencias de interés.

Badnavirus en camote están a muy bajos titulos
Mastrevirus en camote no se sabe que hace, pero esta bajo y tiene muy poca variación.


SPCSV the HIV of sweetpotato 
- no introducir ciertas variantes de un lugar a otro por los tipos de cultivares

Results -----
host &lt;- rep("potato", length(results.ls)) # add the number of samples to study
locations &lt;- c("Cca5", "Czo24", "Czo25", "Czo45", "Czo56", "Hua25", "Ica87", "Jin16") # add the name of the different locations
</code></pre>
<blockquote>
<p>LOCATION KEY<br>
– CZO = Cuzco<br>
– HUA = HUANCAVELICA<br>
– ICA =ICA<br>
– JIN = JUNIN<br>
– CCA =CAJAMARCA</p>
</blockquote>
<pre><code>#--- Generating final data -----
res &lt;- list(NULL)
for (i in seq_along(results.ls)){
	print(i)
	res[[i]] &lt;- dat.parse(results.ls[[i]], locations[i], host[i])
}
</code></pre>
<pre><code>#@------ Create a data frame with all individual information
df &lt;- na.omit(as_tibble(rbind.fill(res)))
#@----- Create summary data and incidence matrix -----
dfs &lt;- ddply(df, .(sampleID, acronym), summarise, cov = mean(`Depth (Norm)`))
dfs &lt;- t(spread(dfs, sampleID, cov, drop=TRUE , fill = 0))
in.mat = t(apply(dfs[2:8,], 1, as.numeric))
colnames(in.mat) &lt;- dfs[1,]
#@--- incidence matrix results
in.mat
</code></pre>
<pre class=" language-r"><code class="prism  language-r"><span class="token comment">#@------------------------ IGRAPH ------------------------</span>
<span class="token comment">#@---- Generating graph object -----</span>
g <span class="token operator">&lt;-</span> graph.incidence<span class="token punctuation">(</span><span class="token keyword">in</span>.mat<span class="token punctuation">,</span> weighted<span class="token operator">=</span> <span class="token boolean">TRUE</span><span class="token punctuation">)</span>

<span class="token comment">#@ Network attributes</span>
V<span class="token punctuation">(</span>g<span class="token punctuation">)</span><span class="token operator">$</span>name <span class="token comment"># Check the vertex names</span>
V<span class="token punctuation">(</span>g<span class="token punctuation">)</span><span class="token operator">$</span>type <span class="token comment"># Check vertex types</span>
<span class="token comment">#@ plotting</span>
plot<span class="token punctuation">(</span>g<span class="token punctuation">,</span> vertex.label.color<span class="token operator">=</span><span class="token string">'black'</span><span class="token punctuation">,</span> vertex.label.dist<span class="token operator">=</span> <span class="token punctuation">(</span><span class="token punctuation">(</span>V<span class="token punctuation">(</span>g<span class="token punctuation">)</span><span class="token operator">$</span>type <span class="token operator">*</span> <span class="token number">4</span><span class="token punctuation">)</span><span class="token operator">-</span><span class="token number">2</span><span class="token punctuation">)</span><span class="token operator">*</span><span class="token operator">-</span><span class="token number">1</span> <span class="token punctuation">,</span> layout <span class="token operator">=</span> layout_as_bipartite<span class="token punctuation">)</span>
plot<span class="token punctuation">(</span>g<span class="token punctuation">,</span> vertex.label.color<span class="token operator">=</span><span class="token string">'black'</span><span class="token punctuation">)</span>
<span class="token comment">#---- Adding attributes</span>
<span class="token comment">#@ Shapes to nodes</span>
shapes <span class="token operator">=</span> c<span class="token punctuation">(</span>rep<span class="token punctuation">(</span><span class="token string">"square"</span><span class="token punctuation">,</span> length<span class="token punctuation">(</span>V<span class="token punctuation">(</span>g<span class="token punctuation">)</span><span class="token operator">$</span>type<span class="token punctuation">[</span>V<span class="token punctuation">(</span>g<span class="token punctuation">)</span><span class="token operator">$</span>type <span class="token operator">==</span> <span class="token string">"FALSE"</span><span class="token punctuation">]</span><span class="token punctuation">)</span><span class="token punctuation">)</span><span class="token punctuation">,</span>
rep<span class="token punctuation">(</span><span class="token string">"circle"</span><span class="token punctuation">,</span> length<span class="token punctuation">(</span>V<span class="token punctuation">(</span>g<span class="token punctuation">)</span><span class="token operator">$</span>type<span class="token punctuation">[</span>V<span class="token punctuation">(</span>g<span class="token punctuation">)</span><span class="token operator">$</span>type <span class="token operator">==</span> <span class="token string">"TRUE"</span><span class="token punctuation">]</span><span class="token punctuation">)</span><span class="token punctuation">)</span><span class="token punctuation">)</span>
<span class="token comment">#@ plotting</span>
plot<span class="token punctuation">(</span>g<span class="token punctuation">,</span> vertex.shape<span class="token operator">=</span>shapes<span class="token punctuation">,</span> vertex.label.color<span class="token operator">=</span><span class="token string">'black'</span><span class="token punctuation">)</span>
<span class="token comment">#------------------------</span>
<span class="token comment">#@ Add colors to nodes</span>
V<span class="token punctuation">(</span>g<span class="token punctuation">)</span><span class="token operator">$</span>color <span class="token operator">&lt;-</span> c<span class="token punctuation">(</span>rep<span class="token punctuation">(</span><span class="token string">"#046A38"</span><span class="token punctuation">,</span> length<span class="token punctuation">(</span>V<span class="token punctuation">(</span>g<span class="token punctuation">)</span><span class="token operator">$</span>type<span class="token punctuation">[</span>V<span class="token punctuation">(</span>g<span class="token punctuation">)</span><span class="token operator">$</span>type <span class="token operator">==</span> <span class="token string">"FALSE"</span><span class="token punctuation">]</span><span class="token punctuation">)</span><span class="token punctuation">)</span><span class="token punctuation">,</span>
rep<span class="token punctuation">(</span><span class="token string">"#FA4616"</span><span class="token punctuation">,</span> length<span class="token punctuation">(</span>V<span class="token punctuation">(</span>g<span class="token punctuation">)</span><span class="token operator">$</span>type<span class="token punctuation">[</span>V<span class="token punctuation">(</span>g<span class="token punctuation">)</span><span class="token operator">$</span>type <span class="token operator">==</span> <span class="token string">"TRUE"</span><span class="token punctuation">]</span><span class="token punctuation">)</span><span class="token punctuation">)</span><span class="token punctuation">)</span>
<span class="token comment">#@ plotting</span>
plot<span class="token punctuation">(</span>g<span class="token punctuation">,</span> vertex.shape<span class="token operator">=</span>shapes<span class="token punctuation">,</span> vertex.label.color<span class="token operator">=</span><span class="token string">'black'</span><span class="token punctuation">)</span>
</code></pre>
<pre><code>#@------------------------ BIPARTITE ------------------------
#library(bipartite)
#@ Plot bipartite network using bipartite package
#@ data in incidence matrix format
plotweb(sortweb(in.mat, sort.order="inc"), method="normal")

#@ Plot in matrix format
visweb(sortweb(in.mat, sort.order="dec"), type= "none", # change to nested or diagonal
labsize= 2, square= "interaction", text= "none", textsize= 4)

#@----- Calculating metrics ----
#@ Node metrics
node.metrics &lt;- specieslevel(round(in.mat))
#@ Exploring metrics
str(node.metrics)
#@ How many levels are in the list?
#@ node.metrics$`higher level` # Want to know about the metrics? Call ?specieslevel
#@ Exploring $`higher level`
(h.nd &lt;- node.metrics$`higher level`[1]) # node degree OR node.metrics$`higher level`$degree
(h.bc &lt;- node.metrics$`higher level`[2]) # species strength
#@ Exploring $`lower level`
(l.nd &lt;- node.metrics$`lower level`[1]) # node degree
(l.bc &lt;- node.metrics$`lower level`[2]) # species strength
#@ Network metrics
network.metrics &lt;- networklevel(round(in.mat))
#@ network.metrics # Want to know about the metrics? Call ?networklevel
#@ Exploring by metric
network.metrics["connectance"] # Connectance
network.metrics["weighted nestedness"] # Nestedness *weighted
#@ Computing modularity
computeModules(in.mat) # Default method: Becket
(modularity &lt;- LPA_wb_plus(in.mat))
mod &lt;- convert2moduleWeb(in.mat, modularity)
plotModuleWeb(mod, weighted = F)
#@----Plotting with attributes ----
#@ Selecting metadata from Virus Detect
meta &lt;- df %&gt;% select(Genus, host, Species, acronym) %&gt;%
			unique()
str(meta)
#@ Adding colors
meta$colors &lt;- ifelse( meta$Genus == "ilarvirus ", "#FA4616",
	ifelse( meta$Genus == 'polerovirus ', "yellow",
		ifelse( meta$Genus == "potexvirus", "brown",
			ifelse( meta$Genus == "nepovirus", "orange",
				ifelse( meta$Genus == "ipomovirus", "red",
					ifelse( meta$Genus == "badnavirus", "gray",
						ifelse( meta$Genus == "potyvirus", "green", "purple")))))))
#@ Combining node attributes from bipartite and igraph
V(g)$color &lt;- c(rep("#046A38", length(V(g)$type[V(g)$type == "FALSE"])),
						meta$colors)
V(g)$xx &lt;- c(unlist(l.nd), unlist(h.bc)+5) # adding node degree + species strength plus a constant
#@ Types of layout algorithms
#@ Default: Kamada-Kawai
plot(g, vertex.shape=shapes, vertex.label=NA, vertex.size= as.numeric(V(g)$xx))
#@ Davidson-Harel
plot(g, vertex.shape=shapes, vertex.size= as.numeric(V(g)$xx), layout=layout_with_dh(g) )
#@ Distributed Recursive
plot(g, vertex.shape=shapes, edge.width= log(E(g)$weight), vertex.size= as.numeric(V(g)$xx), layout=layout_with_drl(g) )
</code></pre>

