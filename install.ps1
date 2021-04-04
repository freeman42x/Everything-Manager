
function Log($what){
  Write-Output "[haskell-editor-setup]: $what"
}


# install choco
if (-Not (Get-Command "choco" -errorAction SilentlyContinue))
{
    Log "installing choco..."
    Set-ExecutionPolicy Bypass -Scope Process -Force
    [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;
    iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))
    Log "choco installed"
}
# install ghc
if (-Not (Get-Command "ghc" -errorAction SilentlyContinue))
{
    Log "installing ghc 8.10.4 through choco..."
    choco install ghc --version=8.10.4
    Log "ghc 8.10.4 installed"
}
if (-Not (Get-Command "cabal" -errorAction SilentlyContinue))
{
    # install cabal
    Log "installing cabal 3.0.0.0 through choco..."
    choco install cabal --version=3.4.0.0
    Log "cabal 3.4.0.0 installed"
}
if (-Not (Get-Command "pg_config" -errorAction SilentlyContinue))
{
    Log "installing postgres13 through choco..."
    choco install postgresql13 --params '/Password:test' --ia '--serverport 5433'
    Log "postgresql13 installed."
}
Log "setup done!"
# TODO Get cachix working
cabal update
cabal install --only-dependencies
cabal build
Log "Setup finished. You can run the project with cabal repl"
