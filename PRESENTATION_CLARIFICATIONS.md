# ============================================================================
# PRESENTATION CLARIFICATIONS SUMMARY
# Addressing Potential Supervisor Questions
# ============================================================================

## PROBLEM IDENTIFIED:
User correctly noted that presentation had unclear points:
1. Why only 3 sectors mentioned but 18 sectors in data?
2. Why only 2 occupational variables?
3. What are the sector/occupation categories?
4. Why these specific groupings?

## CHANGES MADE:

### 1. SLIDE 4 (Data Overview):
**Added:** Explanation of why these dimensions matter
- "18 sectors: Maximum disaggregation (addresses concern)"
- "9 occupations: Full ISCO hierarchy"
- "7 country groups: Welfare regime theory"

### 2. SLIDE 5 (18 Sectors Finding):
**Added:** Methodological note at bottom
- "18 detailed sectors for descriptive analysis (show variation)"
- "4 broad sectors in panel regression (parsimony + avoid multicollinearity)"
→ Clarifies why we show 18 but regress with 4!

### 3. SLIDE 6 (Occupational Moderation):
**Added:** Model specification block at top
- "4 Broad Sectors: Industry (Mining+Manufacturing), Construction, Public (Admin+Education+Health), Services (reference)"
- "2 Occupations: High-Skill (ISCO 1-3) vs. Low-Skill | Managerial (ISCO 1) vs. Non-Managers"

**Added:** Technical details at bottom
- "Reference categories: Services sector, Low-skill/Non-managerial, Year 2010"
- "HC1 cluster-robust SE (controls serial correlation + heteroskedasticity)"

### 4. SLIDE 8 (Beta Convergence):
**Added:** Explanation of beta convergence concept
- "Beta convergence: Growth economics concept—poorer regions catch up"
- "R²=0.517: Initial gaps explain 52% of change—very high for cross-country!"
- "Each 1pp higher 2010 gap → 0.47pp faster reduction"

### 5. BACKUP SLIDES (NEW - 3 slides):

**Backup 1: NACE Sector Classification**
- Shows all 18 NACE codes (B-S) with names
- Shows 4 broad groupings used in regression
- Explains: "18 → multicollinearity, 4 → stable estimates"

**Backup 2: ISCO Occupation Classification**
- Lists all 9 ISCO major groups (1-9)
- Explains High-Skill = ISCO 1-3, Managerial = ISCO 1
- Reason: "Test glass ceiling (managerial) + human capital (skill) separately"

**Backup 3: Theoretical Justification**
- Industry → male-dominated, tournament pay
- Public → formalized scales, transparency
- High-Skill → tests human capital theory (contradicted!)
- Managerial → tests glass ceiling (confirmed, but moderated by sector!)

## KEY CLARIFICATIONS FOR SUPERVISOR:

### Q1: "Why 18 sectors in figures but only 3 in regression results?"
**A:** Actually 4 sectors (Industry, Construction, Public, Services), not 3!
- 18 detailed for DESCRIPTIVE (show variation exists)
- 4 broad for REGRESSION (stable estimates, theory-driven)
- This is standard practice (avoid multicollinearity)

### Q2: "What do 'High-Skill' and 'Managerial' mean exactly?"
**A:** 
- High-Skill = ISCO 1-3 (managers, professionals, technicians) vs. ISCO 4-9
- Managerial = ISCO 1 (top management only) vs. ISCO 2-9
- Two separate binary variables to test different mechanisms

### Q3: "What's included in each sector?"
**A:** Now explicitly stated:
- Industry = Mining (B) + Manufacturing (C)
- Construction = F
- Services = G-N (Trade, Transport, IT, Finance, Professional, Admin)
- Public = O-Q (Public Admin, Education, Health)

### Q4: "Why these groupings?"
**A:** Backup slide 3 explains:
- Theory-driven (institutional, human capital, glass ceiling)
- Industry = male-dominated culture
- Public = transparent pay scales
- High-skill = test productivity theory
- Managerial = test discretionary pay hypothesis

### Q5: "What's beta convergence?"
**A:** Now explained:
- Growth economics concept (poorer catch up to richer)
- Here: high-gap countries catch up to low-gap
- β=-0.474 = 1pp higher initial → 0.47pp faster reduction
- R²=0.517 = 52% variance explained (very strong!)

### Q6: "What are reference categories?"
**A:** Now stated explicitly:
- Sector: Services
- Occupation: Low-skill (ISCO 4-9) + Non-managerial (ISCO 2-9)
- Time: Year 2010
→ All coefficients interpreted relative to these!

## NO MORE AMBIGUITY:

✅ 18 vs 4 sectors → Explained (descriptive vs regression)
✅ Sector composition → Explicitly listed (B-S codes)
✅ Occupation definitions → Clearly defined (ISCO 1-3, ISCO 1)
✅ Reference categories → Stated explicitly
✅ Beta convergence → Defined with interpretation
✅ Why these groupings → Theoretical justification provided
✅ Technical details → SE type, model selection mentioned

## SUPERVISOR-READY:

Presentation now answers:
1. "What data?" → 18 NACE, 9 ISCO, 7 groups (Slide 4)
2. "Why these?" → Maximum disaggregation + theory (Slide 4 + Backup 3)
3. "What's in each category?" → Explicit lists (Slide 6 + Backup 1-2)
4. "Why 18 and 4?" → Descriptive vs. regression (Slide 5 note)
5. "What do coefficients mean?" → Reference categories stated (Slide 6)
6. "What's beta convergence?" → Growth concept explained (Slide 8)

**No more question marks in supervisor's mind!** ✅

============================================================================
