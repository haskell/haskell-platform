
                <div class="container">
                  <h2 id="get-started">Let's get started</h2>
               <p>Note: Users who wish to make use of stack and want  to ensure they are
               running the latest version may want to consider running "stack
               upgrade" and ensuring the
               proper <a href="http://docs.haskellstack.org/en/stable/install_and_upgrade/#path">path</a>
               for stack-installed binaries is in their environment.
                </div>


                <div class="container found-user-platform" >
                    You appear to be using <strong>unknown</strong>.
                    See <a href="#other-platforms">below</a> for other
                    operating systems.
                </div>

                <div class="container unknown-user-platform" >
                    Find your operating system of choice below and follow the
                    instructions to install the Haskell Platform on your system.
                </div>

                <div class="container platform-toc">
                    <ul>
                        <li><a href="#osx"><img src="img/os-osx.svg" alt="Mac OS X logo"> Mac OS X</a></li>
                        <li><a href="#windows"><img src="img/os-windows.svg" alt="Windows logo"> Windows</a></li>
                        <li><a href="#linux"><img src="img/os-linux.svg" alt="Linux logo"> Linux</a></li>
                    </ul>
                </div>

                <div class="container" id="platforms">
                    <h2 id="other-platforms" class="other-platforms">Other Operating Systems</h2>

                    <section class="downloads-platform container" data-os="osx" id="osx">
                        <div class="platform-name"><img src="img/os-osx.svg" alt="Mac OS X logo"><h2>Mac OS X</h2></div>
                        <a class="expander" href="#osx"><div>
                            <img src="img/expand-piece.svg" class="expand-1">
                            <img src="img/expand-piece.svg" class="expand-2">
                            <img src="img/expand-piece.svg" class="expand-3">
                        </div></a>

                        <div class="content">
                          <div id="osx-none" class="flavor active">

                            <p>
				  The recommended way to install the
				  components of the mac platform is
				  using <a href="https://www.haskell.org/ghcup/">ghcup</a>
				  to install ghc and cabal-install,
				  and following the instructions at
				  <a href="https://www.haskellstack.org">haskellstack.org</a>
				  to install stack.
                            </p>
                            </div>
                        </div>
                        <div class="bottom-rule"></div>
                    </section>

                    <section class="downloads-platform container" data-os="windows" id="windows">
                        <div class="platform-name"><img src="img/os-windows.svg" alt="Windows logo"> <h2>Windows</h2></div>
                        <a class="expander" href="#windows"><div>
                            <img src="img/expand-piece.svg" class="expand-1">
                            <img src="img/expand-piece.svg" class="expand-2">
                            <img src="img/expand-piece.svg" class="expand-3">
                        </div></a>
                        <div class="content">
                            <p>
				  The recommended way to install the
				  components of the windows platform is
				  using <a href="https://chocolatey.org">Chocolatey</a>
				  to install ghc and cabal-install,
				  and following the instructions at
				  <a href="https://www.haskellstack.org">haskellstack.org</a>
				  to install stack. Further details
				  for Chocolatey usage are available
				 <a href="https://hub.zhox.com/posts/introducing-haskell-dev/">here</a>.
                            </p>
                            <p> To get started perform these steps:</p>

                            <ol class="install-steps">
                                <li>
                                    <div class="step-number">1</div>
                                    <div class="step-body">
                                      <p><a href="https://chocolatey.org/install">Configure
                                      Chocolatey</a> on your machine.</p>
                                </li>
				<li>
                                    <div class="step-number">2</div>
				    <div class="step-body">
                                         If upgrading from the old-style
					  haskell-platform installer, clean the
					  cabal configuration by
					 running:
                                         <pre>cabal user-config init -f </pre>

                                         Then uninstall prior versions of
                                         the platform.
                                    </div></li>
                                <li>
                                    <div class="step-number">3</div>
                                    <div class="step-body">At an
                                      elevated command prompt, run:
                                      <pre>choco install haskell-dev
refreshenv</pre>
                                </li>

                            </ol>
                        </div>
                        <div class="bottom-rule"></div>
                    </section>

                    <section class="downloads-platform container" data-os="linux" id="linux">
                        <div class="platform-name"><img src="img/os-linux.svg" alt="Linux logo"> <h2>Linux</h2></div>
                        <a class="expander" href="#linux"><div>
                            <img src="img/expand-piece.svg" class="expand-1">
                            <img src="img/expand-piece.svg" class="expand-2">
                            <img src="img/expand-piece.svg" class="expand-3">
                        </div></a>

                        <div class="sidebar flavors">
                          <strong>Choose your distribution</strong>
                                   <p class="select-generic" style="text-align:left">Note that distribution-packaged versions are typically behind the current
                                    platform release. If you prefer to use the latest version rather than the
                                    distribution-packaged version,
                                    then you may want to use the generic Linux installer.</p>
                                </p>

                            <ul>
                                <li><a href="#linux-generic"><span class="logo"><i class="fa fa-cogs"></i></span>Generic</a></li>
                                <li><a href="#linux-ubuntu"><img alt="Ubuntu logo" class="logo" src="img/distro-ubuntu.svg">Ubuntu</a></li>
                                <li><a href="#linux-debian"><img alt="Debian logo" class="logo" src="img/distro-debian.svg">Debian</a></li>
                                <li><a href="#linux-redhat"><img alt="Redhat logo" class="logo" src="img/distro-redhat.svg">Redhat</a></li>
                                <li><a href="#linux-fedora"><img alt="Fedora logo" class="logo" src="img/distro-fedora.svg">Fedora</a></li>
                                <li><a href="#linux-mint"><img alt="Linux Mint logo" class="logo" src="img/distro-mint.svg">Mint</a></li>
                                <li><a href="#linux-gentoo"><img alt="Gentoo Linux logo" class="logo" src="img/distro-gentoo.svg">Gentoo</a></li>
                                <li><a href="#linux-source"><span class="logo"><i class="fa fa-code"></i></span>From Source</a></li>
                            </ul>

                            <p class="select-generic">
                                If you can't find your distribution then select
                                <strong>Generic</strong>.

                            </p>
                        </div>

                        <div class="content">
                            <div id="linux-prompt" class="flavor">
                              <h3>Select your distribution</h3>
			      <p > Please select your distribution on the right.</p>
                                <div class="point-to-flavors-list "><i class="fa fa-arrow-right"></i></div>
                            </div>

                            <div id="linux-generic" class="flavor">
                                <h3>Generic Linux</h3>
                                <p>
                                    This is a <strong>generic</strong>
                                    distribution of the Haskell Platform. While
                                    it should work on most modern Linux
                                    distributions, you may want to use one of the
                                    distribution-specific options listed on the
                                    right. As GHC links against libgmp,
                                    you may need to install "libgmp-dev" using your package manager of choice.
                                </p>
				<p>
				  The recommended way to install the
				  components of the generic platform is
				  using <a href="https://www.haskell.org/ghcup/">ghcup</a>
				  to install ghc and cabal-install,
				  and following the instructions at
				  <a href="https://www.haskellstack.org">haskellstack.org</a>
				  to install stack.
				</p>
<!--                                <p>
                                    The latest version of the Haskell Platform for Linux is
                                    <strong>{{hpVersion}}</strong>.</p>
                                <p> To get started perform these steps:</p>

                                <ol class="install-steps">
                                    <li>
                                        <div class="step-number">1</div>
                                        <div class="step-body">
                                            <p>Download the
                                              installation tarball.</p>
		  <p>The core installer is the recommended
		  installer. It includes all tools. The full installer includes
		  additional global libraries beyond those packaged
		  with ghc. It especially serves those who want full-featured installers in situations
		  where network connectivity should not be taken for
						    granted.</p>
                                            {{#current}} {{#files}} {{#isLinux}}
                                            {{^isFull}}
                                            <div class="download-btn">
                                                <a href="{{> downloads-root}}{{url}}" onclick="return dl(this)" class="btn btn-haskell" role="button">
                                                    <i class="fa fa-download"></i> Download Core ({{archBits}} bit)
                                                </a>
                                            </div>
                                            {{/isFull}}
                                            {{#isFull}}
                                            <div class="download-btn">
                                                <a href="{{> downloads-root}}{{url}}" onclick="return dl(this)" class="btn btn-haskell" role="button">
                                                    <i class="fa fa-download"></i> Download Full ({{archBits}} bit)
                                                </a>
                                            </div>
                                            {{/isFull}}
                                            {{/isLinux}} {{/files}} {{/current}}
                                            <div>
                                                You can verify the integrity of this file by
                                                checking its <strong>SHA-256</strong> hash,
                                                <ul class="hashes">
                                                  {{#current}} {{#files}} {{#isLinux}}
                                                 {{^isFull}}
                                                  <li>{{archBits}} bit Core:<br><textarea rows="2" cols="40" class="file-hash" readonly onclick="this.select()">{{mHash}}</textarea></li>
                                                  {{/isFull}}
                                                  {{#isFull}}
                                                  <li>{{archBits}} bit Full:<br><textarea rows="2" cols="40" class="file-hash" readonly onclick="this.select()">{{mHash}}</textarea></li>
                                                  {{/isFull}}
                                                 {{/isLinux}} {{/files}} {{/current}}
                                                </ul>
                                            </div>
                                        </div>
                                    </li>
                                    <li>
                                        <div class="step-number">2</div>
                                        <div class="step-body">
                                            Install by running:
                                            <pre>$ tar xf ...downloaded archive...
$ sudo ./install-haskell-platform.sh</pre>
                                        </div>
                                    </li>
				    <li>
				      <div class="step-number">3</div>
				      <div class="step-body">
					If you have a system with
					position independent executables
					by default (such as <a href=" https://wiki.ubuntu.com/SecurityTeam/PIE">Ubuntu
					16.10 </a> and above), you
					should edit the GHC settings
					file at
					<pre>usr/local/haskell/ghc-___/lib/ghc-___/settings</pre>
					and change the " compiler
					supports -no-pie" flag from
					"NO" to "YES".
				      </div>
				    </li>
                                </ol>
-->
                            </div><!-- #linux-generic -->

                            <div id="linux-ubuntu" class="flavor">
                                <h3>Ubuntu</h3>
                                <p>Haskell Platform is
                                    available in your distribution's package
                                  <a href="http://packages.ubuntu.com/search?keywords=haskell-platform">repository</a>. Note that distribution-packaged versions are typically behind the current
                                    platform release. If you prefer to use the latest version rather than the
                                    distribution-packaged version,
                                    then you may want to use the generic Linux installer.
				</p>
                                <p>Simply run,</p>
                                <pre>$ sudo apt-get install haskell-platform</pre>
                            </div> <!-- #linux-ubuntu -->

                            <div id="linux-debian" class="flavor">
                                <h3>Debian</h3>
                                <p>Haskell Platform is already
                                    available in your distribution's package
                                    <a href="https://packages.debian.org/search?keywords=haskell-platform">repository</a>. Note that distribution-packaged versions are typically behind the current
                                    platform release. If you prefer to use the latest version rather than the
                                    distribution-packaged version,
                                    then you may want to use the generic Linux installer.</p>
                                <p>Simply run,</p>
                                <pre>$ sudo apt-get install haskell-platform</pre>
                            </div> <!-- #linux-debian -->

                            <div id="linux-mint" class="flavor">
                                <h3>Linux Mint</h3>
                                <p>Haskell Platform is already
                                    available in your distribution's package
                                    <a href="http://community.linuxmint.com/software/view/haskell-platform">repository</a>. Note that distribution-packaged versions are typically behind the current
                                    platform release. If you prefer to use the latest version rather than the
                                    distribution-packaged version,
                                    then you may want to use the generic Linux installer.</p>
                                <p>Simply run,</p>
                                <pre>$ sudo apt-get install haskell-platform</pre>
                            </div> <!-- #linux-mint -->

                            <div id="linux-redhat" class="flavor">
                                <h3>Redhat</h3>
                                <p>Haskell Platform is already available in
                                your distribution's package
                                repository. Note that distribution-packaged versions are typically behind the current
                                    platform release. If you prefer to use the latest version rather than the
                                    distribution-packaged version,
                                    then you may want to use the generic Linux installer.</p>
                                <p>Simply run,</p>
                                <pre>$ sudo yum install haskell-platform</pre>
                            </div> <!-- #linux-redhat -->

                            <div id="linux-fedora" class="flavor">
                                <h3>Fedora</h3>
                                <p>Haskell Platform is already available in
                                    your distribution's package
                                    <a href="https://admin.fedoraproject.org/pkgdb/package/haskell-platform/">repository</a>. Note that distribution-packaged versions are typically behind the current
                                    platform release. If you prefer to use the latest version rather than the
                                    distribution-packaged version,
                                    then you may want to use the generic Linux installer.</p>
                                <p>Simply run,</p>
                                <pre>$ sudo dnf install haskell-platform</pre>
                            </div> <!-- #linux-fedora -->

                            <div id="linux-gentoo" class="flavor">
                                <h3>Gentoo</h3>
                                <p>Haskell Platform is already
                                    available in your distribution's
                                    package repository. Note that distribution-packaged versions are typically behind the current
                                    platform release. If you prefer to use the latest version rather than the
                                    distribution-packaged version,
                                    then you may want to use the generic Linux installer.</p>
                                <p>
                                    While there is a <code>haskell-platform</code> ebuild
                                    included in the main Portage tree,
                                    it is recommended that one uses the more
                                    up-to-date <a href="https://github.com/gentoo-haskell/gentoo-haskell/tree/master/dev-haskell/haskell-platform"><code>gentoo-haskell</code></a>
                                    overlay. This can be done using <code>layman</code>,</p>
                                <pre>
$ sudo layman -a haskell
$ sudo emerge haskell-platform</pre>
                                <p>More details can be found in the
                                    <a href="https://wiki.haskell.org/Gentoo/HaskellPlatform">Wiki</a>.</p>
                            </div> <!-- #linux-gentoo -->

                            <div id="linux-source" class="flavor">
                                <h3>Build from source</h3>
                                <p>
                                    If we don't have a binary package suitable for your distribution
                                    you can build the Haskell Platform from source.
                                </p>
                                <ol class="install-steps">
                                    <li>
                                        <div class="step-number">1</div>
                                        <div class="step-body">
                                            <p>Download and extract the source tarball,</p>
                                            {{#current}} {{#files}} {{#isSource}}
                                            <div class="download-btn">
                                                <a href="{{> downloads-root}}{{url}}" onclick="return dl(this)" class="btn btn-haskell" role="button">
                                                    <i class="fa fa-download"></i> Download
                                                </a>
                                            </div>
                                            {{/isSource}} {{/files}} {{/current}}

                                            <div>
                                                You can verify the integrity of this file by
                                                checking its <strong>SHA-256</strong> hash,
                                                <ul class="hashes">
                                                    {{#current}} {{#files}} {{#isSource}}
                                                    <li><textarea rows="2" cols="40" class="file-hash" readonly onclick="this.select()">{{mHash}}</textarea></li>
                                                    {{/isSource}} {{/files}} {{/current}}
                                                </ul>
                                            </div>
                                        </div>
                                    </li>
                                    <li>
                                        <div class="step-number">2</div>
                                        <div class="step-body">
                                            <p>See the <code>README</code> file for build instructions.</p>
                                        </div>
                                    </li>
                                </ol>
                            </div> <!-- #linux-source -->

                        </div> <!-- linux .content -->
                        <div class="bottom-rule"></div>
                    </section>
                </div>
