self: super:
{
  vim = super.vimHugeX.override { guiSupport = ""; };
}
