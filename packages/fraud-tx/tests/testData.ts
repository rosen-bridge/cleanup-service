export const mockWids = [
  '700577946f4334199916bacf1ce3286404acd4f9476876e6ec7d5bf5a2184d3f',
  '143f7c38a3e10ba24ccb7c4975dd091d0a9b916304c57db6a0534c4b493ce208',
  '76b4eda1bb0e30d47ad77d0da8889b7dafc3410a430426228abb1230fa1a5f30',
  '92932311cd5401cd6690b8da274ff937b8f4196eda94fd45f33aca8d7593eb97',
  '163dd94c65197844769bc5cffbe329a6ca1fef23dcbd2dd0694de846c2094b4b',
  'a482c6a3fc2ee79314f23dd94060fb7b1996319d7c2587545600752376393115',
];

export const triggerEventBoxJson = {
  boxId: '81db498ea4945e0c64f06b64c6ec923df7e7e851b5bf7e22d8d308002e890b59',
  value: '12000000',
  ergoTree:
    '100f0400040004000400040004000400040004000e20666a03af027268e55bb0a8590f5595c7c8c67de49fa57e99b47ebf3969998a29040204000e208fbecc9f4f6968b181d70d337f33729e9b3cb591ffcf8fca16282ccbc487a58704140e2009afe93014f6960aaac3071c919ae18300f15c75a46dd3b22fb1f8c01e65f75bd804d601e4c6a70704d602b4b5a5d9010263d801d604db63087202ed91b172047300938cb27204730100018cb2db6308a77302000173037201d603c2b2a5730400d604cb7203d19683040193b17202720193e4c6a7040ecbb0ad7202d9010563e4c67205040e830002d901053c0e0eb38c7205018c720502af7202d9010563eded93c272057203938cb2db63087205730500018cb2db6308a773060001938cb2db63087205730700029d8cb2db6308a7730800027e72010595937204730996830201938cb2db6308b2a4730a00730b0001730c9299a3730d8cc7a70196830201aea4d901056393cbc27205730e937204e4c6a7060e',
  assets: [
    {
      tokenId:
        '34529f875cad2bf58c5ffb4a9056d26c590f0c35f77958a68dcdb4aa39b437aa',
      amount: '6000',
    },
  ],
  creationHeight: 1628041,
  additionalRegisters: {
    R4: '0e20219ff7e6cd5d693e8c1a813fdc90aab76527420c0685f787326b31844251a632',
  },
  transactionId:
    '2398cc6234d4343e87dc93ef590b85e44c4d8564b459a8486157ea35b9e6716d',
  index: 0,
};

export const cleanerBoxJson = {
  boxId: '489b6ddf6807c3ba36e78c07b0fb6bc9039512a88c3a057fec7eef660020b8f8',
  value: '1100000',
  ergoTree:
    '0008cd0271311d06e39f058ec5c501bf56aec49316c83f5023a435c6b5b29312e40b4c81',
  assets: [
    {
      tokenId:
        '8fbecc9f4f6968b181d70d337f33729e9b3cb591ffcf8fca16282ccbc487a587',
      amount: '2',
    },
  ],
  creationHeight: 1621266,
  additionalRegisters: {},
  transactionId:
    '810f72b0acc98022ef9a4b27235f6c88359d3666f105d2b06cd62514b38caaba',
  index: 2,
};

export const feeBoxesJson = [
  {
    boxId: '5a0c463fac1e120eba9fa5d07862a70cfb98ffb23b0e632bfb40d0caf4ca1ae3',
    value: 4000000000,
    ergoTree:
      '0008cd0271311d06e39f058ec5c501bf56aec49316c83f5023a435c6b5b29312e40b4c81',
    assets: [],
    creationHeight: 1620340,
    additionalRegisters: {},
    transactionId:
      '7f15350cf0d3827985d1134f19843221dda0c4372ba607be4d2ad9eadaefda40',
    index: 0,
  },
];

export const testFraudConfig = {
  fraudAddress:
    '2U1Bm1VfBKJQzNr6Zu6yAh2ZUvdj3LbyQDqtkC3KWmmMoHNwSRTTF1xZ54auFAShAe9Rh1HGkWnGgFiWvGjyYjDjen8j1Qn5mDQGvvrRtR5msj5kbZtiTkLAQ2SB8WBJvW9e4QHdWa2wPnyfe9KMFHMYtcEgdUA2wD4NyvyWNe31R2bxsMykcaxi49WdbWYENRK3WfZ7udYGcsJyKNN2kwpWyyC3ErnLuJNbmeNGFy4QxvKMtcpfZSg',
  cleanerAddress: '9fNurBvmznKrQpYexzF3d9Azdg2NmhQqNCbgBzzs47CR45dpZPv',
  rwtTokenId:
    '34529f875cad2bf58c5ffb4a9056d26c590f0c35f77958a68dcdb4aa39b437aa',
  minBoxValue: 1000000n,
  txFee: '1100000',
};
