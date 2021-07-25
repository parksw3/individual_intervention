## Individual intervention

current: target
-include target.mk
Ignore = target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

## individual_intervention.pdf: individual_intervention.tex
## individual_intervention.tex.pdf: 

Sources += individual_intervention.tex

######################################################################

Sources += $(wildcard *.R)

## Figures are tikz and not pipelined yet 2021 May 04 (Tue)

Rscripts = $(wildcard *.R)
tikzfiles = $(Rscripts:.R=.tex)
Ignore += $(tikzfiles)

autowrapR = defined

%.tex: %.Rout ;

Ignore += scenarios.tex
## scenarios.pdf: scenarios.R

bgsim.Rout: bgsim.R sir-semi.rda
	$(pipeR)

scenarios.Rout: scenarios.R bgsim.rda
	$(pipeR)

######################################################################

## Redeveloping in Phila ☹

figure_beta_gamma.Rout: figure_beta_gamma.R
	$(pipeR)

beta_gamma.Rout: beta_gamma.R sir-semi.rda
	$(pipeR)

beta_gamma_plots.Rout: beta_gamma_plots.R beta_gamma.rda
	$(pipeR)

######################################################################

### Makestuff

## Sources += $(wildcard *.mk)
## include $(wildcard *.mk)

Sources += Makefile
Ignore += makestuff
msrepo = https://github.com/dushoff
Makefile: makestuff/Makefile
makestuff/Makefile:
	git clone $(msrepo)/makestuff
	ls makestuff/Makefile

-include makestuff/os.mk

-include makestuff/pipeR.mk
-include makestuff/texi.mk

-include makestuff/git.mk
-include makestuff/visual.mk
