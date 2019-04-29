self: super:
{
  vim = super.vimHugeX.override { guiSupport = "gtk2"; };
}
