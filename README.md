## OI.Transover

## Study Objective
Overimitation is hypothesized to drive cumulative culture by fostering the adoption and transmission of information within populations. This study tests this claim by assigning 5-year-old children (N = 64) to one of two study populations based on their tendency to overimitate (overimitators/OIs vs. non-overimitators/non-OIs). Children were presented with conventional information in the form of novel games. Both study populations showed little variation in their persistence to adopt and transmit elements of these games. However, OIs were more likely to use generic language than non-OIs when transmitting game information to their peers. Further, non-OIs altered the game rules more frequently, suggesting differences in children’s tendency to innovate between study populations. These findings indicate subtle links between cumulative culture and children’s overimitation.

## File Structure
- OI_Transover.Rproj

- data
    - R-Transover.xls

- functions (all functions are written and provided by Roger Mundry)
    - boot_glmm.r             
    - diagnostic_fcns.r       
    - drop1_para_glmmtmb.r    
    - glmm_stability.r
    - boot_glmmTMB.r          
    - drop1_para.r            
    - glmmTMB_stability.r
    
    All functions are needed to run the code

- Analisis.1.adoption.transmission.fidelity
    - Analisis.1.adoption.transmission.fidelity.R
    - Analisis.1.adoption.transmission.fidelity.RData
    - Analisis.1.Plot.adoption.transmission.fidelity.R
    - Analisis.1.Plot.adoption.transmission.fidelity.RData

- Analysis.2.generic.language
    - Analysis.2.generic.language.R
    - Analysis.2.generic.language.RData
    - Analysis.2.Plot.generic.language.R     
    - Analysis.2.Plot.generic.language.RData  

- Analysis.3.game.modification
   - Analysis.3.game.modification.R
   - Analysis.3.game.modification.RData
   - Analysis.3.Plot.game.modification.R  
   - Analysis.3.Plot.game.modification.RData 
   
# Data dictionary (R-Transover.xls):
  - id: participant id 
  - Sex: participant's sex
  - Age: age in days
  - Study.population: OI if children overimitated in both tasks
                      non-OI if children did not overimitate in both tasks
  - Game: type of game that was presented during that study phase (1: square, 2: seesaw)
  - Study.phase: first study phase: children saw a video of adult
                 second study phase: children saw a video of another child recorded in phase 1
  - Fidelity.type: game adoption or game transmission
  - Fidelity: Fidelity Score for game adoption or game transmission which can reach from 0 to 11
  - Generic.Language.Score: count of normative utterances
  - Innovation: Game modification score which could reach from 0 to 3
  - video.nr: number of the associated video
  - Language.Transcription.complete: All of the children's verbal instruction during game transmission
  - Language.Transcription.generic: All of the children's generic verbal instruction during game transmission
  - Comments: comments of the coder 
