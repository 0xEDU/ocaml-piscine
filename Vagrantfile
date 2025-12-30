# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  # ---------- BOX ----------
  config.vm.box = "debian/bookworm64"  # lightweight Debian box

  # ---------- SHARED FOLDER ----------
  # The ./shared folder in your project maps to /home/vagrant/shared in the VM
  config.vm.synced_folder "../ocaml-piscine", "/home/vagrant/ocaml-piscine"

  # ---------- PROVIDER SETTINGS ----------
  config.vm.provider "virtualbox" do |vb|
    vb.name = "ocaml_vm"
    vb.memory = "2048"
    vb.cpus = 2
  end

  # ---------- PROVISIONING ----------
  # Install OCaml and initialize OPAM
  config.vm.provision "shell", privileged: false, inline: <<-SHELL
    set -e

    # System deps
    sudo apt-get update
    sudo apt-get install -y curl git build-essential bubblewrap unzip

    # Install opam non-interactively
    sudo curl -sS -L -o /usr/bin/opam \
      https://github.com/ocaml/opam/releases/download/2.5.0/opam-2.5.0-x86_64-linux \
      && sudo chmod +x /usr/bin/opam

    # Initialize opam
    opam init -y --disable-sandboxing
    eval $(opam env)

    # Create OCaml switch
    opam switch create 4.14.0
    eval $(opam env)

    # Persist opam env for future shells
    echo 'eval $(opam env)' >> ~/.bashrc
  SHELL
end
