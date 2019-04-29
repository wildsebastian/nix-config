self: super:
{
  gtk3 = super.gtk3.override { x11Support = true; };
  vim = super.vim;
}
