{
  lib,
  python3,
  src,
  ...
}:

python3.pkgs.buildPythonApplication rec {
  pname = "riemann-client";
  version = "7.0.0";
  pyproject = true;

  inherit src;

  nativeBuildInputs = [
    python3.pkgs.setuptools
    python3.pkgs.wheel
  ];

  propagatedBuildInputs = with python3.pkgs; [
    click
    protobuf
  ];

  passthru.optional-dependencies = with python3.pkgs; {
    docs = [
      sphinx
      sphinx-rtd-theme
    ];
  };

  pythonImportsCheck = [ "riemann_client" ];

  meta = with lib; {
    description = "A Riemann client and command line tool";
    homepage = "https://pypi.org/project/riemann-client/";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
    mainProgram = "riemann-client";
  };
}
