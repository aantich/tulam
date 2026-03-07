module MetadataResolver (resolveTargetImports) where

import State
import Surface (ModulePath, Name)

-- | Given target import declarations, resolve class metadata from external sources.
-- This is a codegen-time operation. In the interpreter, this is a no-op.
-- When codegen backends are implemented, this will:
--   dotnet: read .NET assembly metadata via Mono.Cecil
--   js:     parse .d.ts TypeScript declaration files
--   native: parse C/C++ headers via libclang
resolveTargetImports :: [(ModulePath, Name)] -> Environment -> IO Environment
resolveTargetImports _ env = pure env  -- stub: no resolution in interpreter
