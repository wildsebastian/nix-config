self: super:
{
  vim_configurable = super.vim_configurable.override {
    guiSupport = "no";
  };
}
