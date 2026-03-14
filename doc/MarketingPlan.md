# tulam Marketing & Promotion Plan

## Core Messaging

**Primary hook**: "A functional language with dependent types, algebraic effects, and native codegen within 1.6x of C++ — beating it on 4 out of 9 AWFY benchmarks."

**Key differentiators to emphasize**:
- Beats C++ on Queens (5x faster), Permute (2x), Mandelbrot, Storage
- Most expressive type system of any compiled language targeting native: dependent types (Pi + Sigma), algebraic effects, row polymorphism, universe hierarchy — all in one language
- Multi-target: same source compiles to LLVM native, JS, and .NET
- Two primitives (tuples + lambdas) — everything else derived
- Categorical vocabulary: `algebra`, `morphism`, `extends`, laws as code
- First-class OOP with 1-1 codegen mapping (not an encoding)
- Algebra-based interop — no FFI glue code

**Tagline options**:
- "The most expressive type system. Every backend. Native performance."
- "Dependent types meet native performance."
- "Types without compromise."

---

## Tier 1 — High Impact (Launch Week)

### Hacker News (news.ycombinator.com)
- **Audience**: ~500k daily, heavily technical, loves PLs and benchmarks
- **Format**: "Show HN" post
- **Title**: `Show HN: tulam – functional language with dependent types, 1.6x of C++ on AWFY benchmarks`
- **Link to**: tulam.org
- **Timing**: Tuesday–Thursday, 9–11am EST (peak engagement window)
- **Tips**:
  - First comment should be a detailed "what and why" from the author
  - Mention what's working now (interpreter, native backend, 1050+ tests) and what's coming (JS/.NET targets)
  - Be honest about current status — HN respects that
  - Respond to every comment in the first 2 hours — this keeps the post alive
  - Avoid superlatives in the title; let the numbers speak

### Reddit — r/ProgrammingLanguages (~130k members)
- **The** community for new language announcements
- **Post as**: a self-post with overview, code examples, benchmark table, and link to site
- **Flair**: "Language announcement" or "Show & Tell"
- **What they care about**: type system design decisions, novel ideas, honest tradeoffs
- **What to include**:
  - Why tuples + lambdas as primitives?
  - How does the categorical vocabulary (algebra/morphism) differ from Haskell typeclasses?
  - What does the compilation pipeline look like?
  - Benchmark methodology and caveats
- **Expect**: deep technical questions about type theory, comparisons to Lean/Idris/Agda
- **Do first** — use feedback to refine messaging before HN

### Reddit — r/haskell (~90k members)
- **Angle**: "I built a dependently-typed language in Haskell that compiles to native and beats C++"
- **What they care about**: type system expressiveness, compilation approach, how it relates to GHC
- **Include**: comparison to Haskell (what tulam does differently and why)
- **Timing**: post 1-2 days after r/ProgrammingLanguages

### Reddit — r/functionalprogramming (~60k members)
- **Angle**: broader, more accessible — focus on the language feel and examples
- **Cross-post** from r/ProgrammingLanguages or write a gentler intro

### Reddit — r/compilers (~30k members)
- **Angle**: the LLVM backend, Perceus RC, how a functional language achieves near-C++ performance
- **What they care about**: codegen strategy, optimization passes, benchmark methodology

---

## Tier 2 — Community & Discussion (Week 1-2)

### Lobsters (lobste.rs)
- Curated HN alternative, very PL-friendly
- Requires an invitation from an existing member
- Tag: `plt`, `compilers`, `release`
- Tends to have higher-quality, more focused discussion than HN
- **Action**: find a Lobsters member to invite you, or ask on Twitter/Mastodon

### Lambda the Ultimate (lambda-the-ultimate.org)
- The classic programming languages research blog/forum
- Academic-leaning audience, deeply knowledgeable
- Post in the forum section with a technical overview
- Focus on: type theory foundations, the algebra/morphism distinction, dependent types in a compiled systems language
- Slower-paced discussion but very influential in PL circles

### Haskell Discourse (discourse.haskell.org)
- Post in "Show and Tell" category
- Emphasize the Haskell implementation, what you learned, interesting Haskell patterns used
- The community loves seeing ambitious projects built in Haskell

### OCaml Discuss (discuss.ocaml.org)
- tulam shares some design philosophy with OCaml (ML-style, practical FP)
- Angle: how tulam's approach to effects/modules differs from OCaml 5

### Zulip — Lean Community (leanprover.zulipchat.com)
- Lean users care deeply about dependent types + performance
- Angle: how tulam makes different tradeoffs (systems language vs proof assistant)

---

## Tier 3 — Social Media (Ongoing)

### X / Twitter
- **Thread format**: 8-10 tweets with code screenshots and benchmark chart
- **Tag**: @plt_proofs, @typesafety, @haborym (Haskell community accounts)
- **Hashtags**: #ProgrammingLanguages #TypeTheory #FunctionalProgramming #CompilerDesign #LLVM
- **Content ideas**:
  - "Here's a functional language that beats C++ on AWFY benchmarks" + screenshot
  - Short code example threads ("tulam in 60 seconds")
  - Behind-the-scenes: "How we got 0.2x C++ on Queens with Perceus RC"
  - Benchmark evolution over time (show progress)

### Mastodon
- **Instance**: `types.pl` — literally a PL theory Mastodon server
- **Also post on**: `fosstodon.org` (FOSS community), `functional.cafe`
- Similar thread format to Twitter
- PL Mastodon is very engaged and supportive of new projects

### Bluesky
- Growing PL/tech community
- Cross-post Twitter threads

---

## Tier 4 — Long-Form Content (Weeks 2-4)

### Blog Posts

**Post 1: "Why I built tulam" (launch post)**
- Personal motivation, design philosophy
- The "two primitives" insight
- Where it fits in the PL landscape
- Publish on: tulam.org/blog, cross-post to Dev.to, Hashnode, Medium

**Post 2: "How a functional language beats C++ on AWFY benchmarks"**
- Deep dive into codegen: Perceus RC, defunctionalization, monomorphization
- Benchmark methodology, what each benchmark tests
- Honest analysis of where tulam is still slower and why
- This is your HN/Reddit viral post

**Post 3: "Algebras, morphisms, and laws — a categorical type system for real code"**
- The algebra/morphism distinction vs Haskell typeclasses
- Laws as first-class compiler objects
- Auto-composition of morphisms
- Target audience: PL researchers and Haskell/Lean users

**Post 4: "One language, every target"**
- The multi-target architecture
- Algebra-based interop vs FFI
- Target-qualified instances
- How the same code maps to .NET classes, JS objects, and LLVM IR

### Platforms for blog posts
- **Dev.to** (~1M monthly) — large developer audience, good SEO
- **Hashnode** — developer blogging, custom domain support
- **Medium** — broader reach but paywalled; use for cross-posts only
- **tulam.org/blog** — canonical source (add a blog section to the site)

---

## Tier 5 — Academic & Research (Month 2+)

### Papers

**Option A: arXiv preprint**
- Title: "tulam: Dependent Types with Native Performance via Perceus RC and Defunctionalization"
- Sections: motivation, type system overview, compilation pipeline, benchmark evaluation, related work
- ~10-15 pages, can be written incrementally
- Immediate visibility to PL researchers via arXiv alerts
- No peer review needed — just upload

**Option B: Workshop paper**
- **TyDe** (Type-Driven Development) — co-located with ICFP, very welcoming to new languages
- **ML Workshop** — co-located with ICFP, focuses on ML-family languages
- **Haskell Symposium** — natural fit for "ambitious project built in Haskell"
- **HOPE** (Higher-Order Programming with Effects) — effect system angle
- These are typically 6-12 pages, lighter review process

**Option C: Conference paper (stretch goal)**
- **ICFP** experience report track — "experience building a dependently-typed compiled language"
- **PLDI** — if you have novel compilation results worth publishing
- **POPL** — if the type theory has novel contributions
- Full papers are 20-25 pages with rigorous evaluation

### Academic Outreach
- Email PL researchers whose work influenced tulam (they love hearing about this)
- Present at university PL seminars (many are open to external speakers)
- Submit to **SIGPLAN-M** mentorship program if you want academic guidance

---

## Tier 6 — Multimedia (Month 1-2)

### YouTube
- **Video 1**: "tulam in 10 minutes" — REPL demo, type system showcase, benchmark results
- **Video 2**: "Building a compiler that beats C++" — deeper technical walkthrough
- **Video 3**: "Dependent types for working programmers" — tutorial angle
- PL YouTube is underserved — good videos get shared widely
- Cross-post to: Twitter, Reddit, HN

### Podcasts (pitch as a guest)
- **CoRecursive** (Adam Gordon Bell) — deep technical interviews, perfect fit
- **The Haskell Interlude** — Haskell community podcast
- **Functional Geekery** — FP-focused podcast
- **Type Theory Forall** — academic PL podcast
- **Developer Voices** — broader dev audience
- **Pitch angle**: "I built a language with the most expressive type system that compiles faster than C++"

### Conference Talks
- **Strange Loop** (if it returns) — loves ambitious PL projects
- **ICFP** — give a talk if paper is accepted
- **Lambda Days** / **Code BEAM** — FP conference circuit
- **local meetups**: Haskell meetups, FP meetups, PL reading groups

---

## Launch Sequence (Recommended Order)

### Week 0 (Preparation)
- [ ] Finalize tulam.org — ensure all sections look polished
- [ ] Prepare a detailed "about" comment for HN (500-800 words)
- [ ] Create a Twitter/Mastodon thread draft with screenshots
- [ ] Write a self-post draft for r/ProgrammingLanguages
- [ ] Record a short (3-5 min) demo video or GIF for social media

### Week 1 (Soft Launch)
- [ ] **Monday**: Post on r/ProgrammingLanguages
- [ ] **Monday**: Post on Mastodon (types.pl)
- [ ] **Tuesday-Wednesday**: Gather feedback, refine messaging
- [ ] **Thursday**: Post on r/haskell and r/functionalprogramming

### Week 2 (Main Launch)
- [ ] **Tuesday 10am EST**: Show HN post
- [ ] **Same day**: Twitter thread, r/compilers
- [ ] **Wednesday**: Lobsters (if you have access), Lambda the Ultimate
- [ ] **Thursday**: Haskell Discourse, OCaml Discuss
- [ ] **Friday**: Dev.to blog post ("Why I built tulam")

### Week 3-4 (Momentum)
- [ ] Technical blog post: "How a functional language beats C++"
- [ ] YouTube demo video
- [ ] Pitch podcasts
- [ ] Start arXiv preprint draft

### Month 2+
- [ ] Academic workshop submissions (check deadlines)
- [ ] Conference talk proposals
- [ ] Ongoing social media: share progress updates, new benchmarks, new features

---

## Metrics to Track

- **GitHub stars** — vanity but correlates with awareness
- **tulam.org unique visitors** (Google Analytics is set up)
- **HN points and comment count**
- **Reddit upvotes and engagement**
- **GitHub issues/PRs from external contributors**
- **Twitter/Mastodon impressions and followers**

---

## Content Assets to Prepare

- [ ] Benchmark chart as a standalone image (for social media sharing)
- [ ] 3-4 polished code example screenshots (dark theme, syntax highlighted)
- [ ] Comparison table: tulam vs Haskell vs Lean vs Rust vs OCaml (features matrix)
- [ ] Architecture diagram (source → CLM → LLVM/JS/.NET)
- [ ] Short GIF/video of REPL session
- [ ] "tulam in 30 seconds" code snippet that shows off the best features in minimal lines
