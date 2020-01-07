(program
  (let
    (nonrec)
    (datatypebind
      (datatype (tyvardecl Unit (type))  Unit_match (vardecl Unit Unit))
    )
    (let
      (nonrec)
      (termbind (strict) (vardecl fAdditiveGroupValue (con integer)) (con -1))
      (let
        (nonrec)
        (datatypebind
          (datatype
            (tyvardecl Tuple2 (fun (type) (fun (type) (type))))
            (tyvardecl a (type)) (tyvardecl b (type))
            Tuple2_match
            (vardecl Tuple2 (fun a (fun b [[Tuple2 a] b])))
          )
        )
        (let
          (rec)
          (datatypebind
            (datatype
              (tyvardecl List (fun (type) (type)))
              (tyvardecl a (type))
              Nil_match
              (vardecl Nil [List a])
              (vardecl Cons (fun a (fun [List a] [List a])))
            )
          )
          (let
            (nonrec)
            (termbind
              (strict)
              (vardecl
                multiplyInteger
                (fun (con integer) (fun (con integer) (con integer)))
              )
              (builtin multiplyInteger)
            )
            (let
              (nonrec)
              (termbind
                (strict)
                (vardecl
                  fAdditiveGroupValue_cscale
                  (fun (con integer) (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]))
                )
                (lam
                  i
                  (con integer)
                  (lam
                    ds
                    [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                    (let
                      (rec)
                      (termbind
                        (strict)
                        (vardecl
                          go
                          (fun [List [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]] [List [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]])
                        )
                        (lam
                          ds
                          [List [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]]
                          [
                            [
                              [
                                {
                                  [
                                    {
                                      Nil_match
                                      [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                    }
                                    ds
                                  ]
                                  (fun Unit [List [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]])
                                }
                                (lam
                                  thunk
                                  Unit
                                  {
                                    Nil
                                    [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                  }
                                )
                              ]
                              (lam
                                ds
                                [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                (lam
                                  xs
                                  [List [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]]
                                  (lam
                                    thunk
                                    Unit
                                    [
                                      {
                                        [
                                          {
                                            { Tuple2_match (con bytestring) }
                                            [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]
                                          }
                                          ds
                                        ]
                                        [List [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]]
                                      }
                                      (lam
                                        c
                                        (con bytestring)
                                        (lam
                                          i
                                          [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]
                                          [
                                            [
                                              {
                                                Cons
                                                [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                              }
                                              [
                                                [
                                                  {
                                                    { Tuple2 (con bytestring) }
                                                    [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]
                                                  }
                                                  c
                                                ]
                                                (let
                                                  (rec)
                                                  (termbind
                                                    (strict)
                                                    (vardecl
                                                      go
                                                      (fun [List [[Tuple2 (con bytestring)] (con integer)]] [List [[Tuple2 (con bytestring)] (con integer)]])
                                                    )
                                                    (lam
                                                      ds
                                                      [List [[Tuple2 (con bytestring)] (con integer)]]
                                                      [
                                                        [
                                                          [
                                                            {
                                                              [
                                                                {
                                                                  Nil_match
                                                                  [[Tuple2 (con bytestring)] (con integer)]
                                                                }
                                                                ds
                                                              ]
                                                              (fun Unit [List [[Tuple2 (con bytestring)] (con integer)]])
                                                            }
                                                            (lam
                                                              thunk
                                                              Unit
                                                              {
                                                                Nil
                                                                [[Tuple2 (con bytestring)] (con integer)]
                                                              }
                                                            )
                                                          ]
                                                          (lam
                                                            ds
                                                            [[Tuple2 (con bytestring)] (con integer)]
                                                            (lam
                                                              xs
                                                              [List [[Tuple2 (con bytestring)] (con integer)]]
                                                              (lam
                                                                thunk
                                                                Unit
                                                                [
                                                                  {
                                                                    [
                                                                      {
                                                                        {
                                                                          Tuple2_match
                                                                          (con bytestring)
                                                                        }
                                                                        (con integer)
                                                                      }
                                                                      ds
                                                                    ]
                                                                    [List [[Tuple2 (con bytestring)] (con integer)]]
                                                                  }
                                                                  (lam
                                                                    c
                                                                    (con bytestring)
                                                                    (lam
                                                                      i
                                                                      (con integer)
                                                                      [
                                                                        [
                                                                          {
                                                                            Cons
                                                                            [[Tuple2 (con bytestring)] (con integer)]
                                                                          }
                                                                          [
                                                                            [
                                                                              {
                                                                                {
                                                                                  Tuple2
                                                                                  (con bytestring)
                                                                                }
                                                                                (con integer)
                                                                              }
                                                                              c
                                                                            ]
                                                                            [
                                                                              [
                                                                                multiplyInteger
                                                                                i
                                                                              ]
                                                                              i
                                                                            ]
                                                                          ]
                                                                        ]
                                                                        [
                                                                          go xs
                                                                        ]
                                                                      ]
                                                                    )
                                                                  )
                                                                ]
                                                              )
                                                            )
                                                          )
                                                        ]
                                                        Unit
                                                      ]
                                                    )
                                                  )
                                                  [ go i ]
                                                )
                                              ]
                                            ]
                                            [ go xs ]
                                          ]
                                        )
                                      )
                                    ]
                                  )
                                )
                              )
                            ]
                            Unit
                          ]
                        )
                      )
                      [ go ds ]
                    )
                  )
                )
              )
              (let
                (nonrec)
                (termbind
                  (strict)
                  (vardecl
                    addInteger
                    (fun (con integer) (fun (con integer) (con integer)))
                  )
                  (builtin addInteger)
                )
                (let
                  (nonrec)
                  (datatypebind
                    (datatype
                      (tyvardecl These (fun (type) (fun (type) (type))))
                      (tyvardecl a (type)) (tyvardecl b (type))
                      These_match
                      (vardecl That (fun b [[These a] b]))
                      (vardecl These (fun a (fun b [[These a] b])))
                      (vardecl This (fun a [[These a] b]))
                    )
                  )
                  (let
                    (nonrec)
                    (datatypebind
                      (datatype
                        (tyvardecl Bool (type))
                        
                        Bool_match
                        (vardecl True Bool) (vardecl False Bool)
                      )
                    )
                    (let
                      (nonrec)
                      (termbind
                        (strict)
                        (vardecl
                          equalsByteString
                          (fun (con bytestring) (fun (con bytestring) Bool))
                        )
                        (lam
                          arg
                          (con bytestring)
                          (lam
                            arg
                            (con bytestring)
                            [
                              (lam
                                b
                                (all a (type) (fun a (fun a a)))
                                [ [ { b Bool } True ] False ]
                              )
                              [ [ (builtin equalsByteString) arg ] arg ]
                            ]
                          )
                        )
                      )
                      (let
                        (rec)
                        (termbind
                          (strict)
                          (vardecl
                            fFunctorNil_cfmap
                            (all a (type) (all b (type) (fun (fun a b) (fun [List a] [List b]))))
                          )
                          (abs
                            a
                            (type)
                            (abs
                              b
                              (type)
                              (lam
                                f
                                (fun a b)
                                (lam
                                  l
                                  [List a]
                                  [
                                    [
                                      [
                                        {
                                          [ { Nil_match a } l ]
                                          (fun Unit [List b])
                                        }
                                        (lam thunk Unit { Nil b })
                                      ]
                                      (lam
                                        x
                                        a
                                        (lam
                                          xs
                                          [List a]
                                          (lam
                                            thunk
                                            Unit
                                            [
                                              [ { Cons b } [ f x ] ]
                                              [
                                                [
                                                  { { fFunctorNil_cfmap a } b }
                                                  f
                                                ]
                                                xs
                                              ]
                                            ]
                                          )
                                        )
                                      )
                                    ]
                                    Unit
                                  ]
                                )
                              )
                            )
                          )
                        )
                        (let
                          (rec)
                          (termbind
                            (strict)
                            (vardecl
                              foldr
                              (all a (type) (all b (type) (fun (fun a (fun b b)) (fun b (fun [List a] b)))))
                            )
                            (abs
                              a
                              (type)
                              (abs
                                b
                                (type)
                                (lam
                                  f
                                  (fun a (fun b b))
                                  (lam
                                    acc
                                    b
                                    (lam
                                      l
                                      [List a]
                                      [
                                        [
                                          [
                                            {
                                              [ { Nil_match a } l ] (fun Unit b)
                                            }
                                            (lam thunk Unit acc)
                                          ]
                                          (lam
                                            x
                                            a
                                            (lam
                                              xs
                                              [List a]
                                              (lam
                                                thunk
                                                Unit
                                                [
                                                  [ f x ]
                                                  [
                                                    [
                                                      [ { { foldr a } b } f ]
                                                      acc
                                                    ]
                                                    xs
                                                  ]
                                                ]
                                              )
                                            )
                                          )
                                        ]
                                        Unit
                                      ]
                                    )
                                  )
                                )
                              )
                            )
                          )
                          (let
                            (nonrec)
                            (termbind
                              (strict)
                              (vardecl
                                union
                                (all k (type) (all v (type) (all r (type) (fun [(lam a (type) (fun a (fun a Bool))) k] (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) k] v] (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) k] r] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) k] [[These v] r]]))))))
                              )
                              (abs
                                k
                                (type)
                                (abs
                                  v
                                  (type)
                                  (abs
                                    r
                                    (type)
                                    (lam
                                      dEq
                                      [(lam a (type) (fun a (fun a Bool))) k]
                                      (lam
                                        ds
                                        [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) k] v]
                                        (lam
                                          ds
                                          [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) k] r]
                                          [
                                            [
                                              [
                                                {
                                                  {
                                                    foldr
                                                    [[Tuple2 k] [[These v] r]]
                                                  }
                                                  [List [[Tuple2 k] [[These v] r]]]
                                                }
                                                {
                                                  Cons
                                                  [[Tuple2 k] [[These v] r]]
                                                }
                                              ]
                                              [
                                                [
                                                  {
                                                    {
                                                      fFunctorNil_cfmap
                                                      [[Tuple2 k] r]
                                                    }
                                                    [[Tuple2 k] [[These v] r]]
                                                  }
                                                  (lam
                                                    ds
                                                    [[Tuple2 k] r]
                                                    [
                                                      {
                                                        [
                                                          {
                                                            { Tuple2_match k } r
                                                          }
                                                          ds
                                                        ]
                                                        [[Tuple2 k] [[These v] r]]
                                                      }
                                                      (lam
                                                        c
                                                        k
                                                        (lam
                                                          b
                                                          r
                                                          [
                                                            [
                                                              {
                                                                { Tuple2 k }
                                                                [[These v] r]
                                                              }
                                                              c
                                                            ]
                                                            [
                                                              { { That v } r } b
                                                            ]
                                                          ]
                                                        )
                                                      )
                                                    ]
                                                  )
                                                ]
                                                [
                                                  [
                                                    [
                                                      {
                                                        { foldr [[Tuple2 k] r] }
                                                        [List [[Tuple2 k] r]]
                                                      }
                                                      (lam
                                                        e
                                                        [[Tuple2 k] r]
                                                        (lam
                                                          xs
                                                          [List [[Tuple2 k] r]]
                                                          (let
                                                            (nonrec)
                                                            (termbind
                                                              (strict)
                                                              (vardecl
                                                                wild
                                                                [[Tuple2 k] r]
                                                              )
                                                              e
                                                            )
                                                            [
                                                              {
                                                                [
                                                                  {
                                                                    {
                                                                      Tuple2_match
                                                                      k
                                                                    }
                                                                    r
                                                                  }
                                                                  e
                                                                ]
                                                                [List [[Tuple2 k] r]]
                                                              }
                                                              (lam
                                                                c
                                                                k
                                                                (lam
                                                                  ds
                                                                  r
                                                                  [
                                                                    [
                                                                      [
                                                                        {
                                                                          [
                                                                            Bool_match
                                                                            [
                                                                              [
                                                                                [
                                                                                  {
                                                                                    {
                                                                                      foldr
                                                                                      [[Tuple2 k] v]
                                                                                    }
                                                                                    Bool
                                                                                  }
                                                                                  (lam
                                                                                    a
                                                                                    [[Tuple2 k] v]
                                                                                    (lam
                                                                                      acc
                                                                                      Bool
                                                                                      [
                                                                                        [
                                                                                          [
                                                                                            {
                                                                                              [
                                                                                                Bool_match
                                                                                                acc
                                                                                              ]
                                                                                              (fun Unit Bool)
                                                                                            }
                                                                                            (lam
                                                                                              thunk
                                                                                              Unit
                                                                                              True
                                                                                            )
                                                                                          ]
                                                                                          (lam
                                                                                            thunk
                                                                                            Unit
                                                                                            [
                                                                                              {
                                                                                                [
                                                                                                  {
                                                                                                    {
                                                                                                      Tuple2_match
                                                                                                      k
                                                                                                    }
                                                                                                    v
                                                                                                  }
                                                                                                  a
                                                                                                ]
                                                                                                Bool
                                                                                              }
                                                                                              (lam
                                                                                                c
                                                                                                k
                                                                                                (lam
                                                                                                  ds
                                                                                                  v
                                                                                                  [
                                                                                                    [
                                                                                                      dEq
                                                                                                      c
                                                                                                    ]
                                                                                                    c
                                                                                                  ]
                                                                                                )
                                                                                              )
                                                                                            ]
                                                                                          )
                                                                                        ]
                                                                                        Unit
                                                                                      ]
                                                                                    )
                                                                                  )
                                                                                ]
                                                                                False
                                                                              ]
                                                                              ds
                                                                            ]
                                                                          ]
                                                                          (fun Unit [List [[Tuple2 k] r]])
                                                                        }
                                                                        (lam
                                                                          thunk
                                                                          Unit
                                                                          xs
                                                                        )
                                                                      ]
                                                                      (lam
                                                                        thunk
                                                                        Unit
                                                                        [
                                                                          [
                                                                            {
                                                                              Cons
                                                                              [[Tuple2 k] r]
                                                                            }
                                                                            wild
                                                                          ]
                                                                          xs
                                                                        ]
                                                                      )
                                                                    ]
                                                                    Unit
                                                                  ]
                                                                )
                                                              )
                                                            ]
                                                          )
                                                        )
                                                      )
                                                    ]
                                                    { Nil [[Tuple2 k] r] }
                                                  ]
                                                  ds
                                                ]
                                              ]
                                            ]
                                            [
                                              [
                                                {
                                                  {
                                                    fFunctorNil_cfmap
                                                    [[Tuple2 k] v]
                                                  }
                                                  [[Tuple2 k] [[These v] r]]
                                                }
                                                (lam
                                                  ds
                                                  [[Tuple2 k] v]
                                                  [
                                                    {
                                                      [
                                                        { { Tuple2_match k } v }
                                                        ds
                                                      ]
                                                      [[Tuple2 k] [[These v] r]]
                                                    }
                                                    (lam
                                                      c
                                                      k
                                                      (lam
                                                        i
                                                        v
                                                        [
                                                          [
                                                            {
                                                              { Tuple2 k }
                                                              [[These v] r]
                                                            }
                                                            c
                                                          ]
                                                          (let
                                                            (rec)
                                                            (termbind
                                                              (strict)
                                                              (vardecl
                                                                go
                                                                (fun [List [[Tuple2 k] r]] [[These v] r])
                                                              )
                                                              (lam
                                                                ds
                                                                [List [[Tuple2 k] r]]
                                                                [
                                                                  [
                                                                    [
                                                                      {
                                                                        [
                                                                          {
                                                                            Nil_match
                                                                            [[Tuple2 k] r]
                                                                          }
                                                                          ds
                                                                        ]
                                                                        (fun Unit [[These v] r])
                                                                      }
                                                                      (lam
                                                                        thunk
                                                                        Unit
                                                                        [
                                                                          {
                                                                            {
                                                                              This
                                                                              v
                                                                            }
                                                                            r
                                                                          }
                                                                          i
                                                                        ]
                                                                      )
                                                                    ]
                                                                    (lam
                                                                      ds
                                                                      [[Tuple2 k] r]
                                                                      (lam
                                                                        xs
                                                                        [List [[Tuple2 k] r]]
                                                                        (lam
                                                                          thunk
                                                                          Unit
                                                                          [
                                                                            {
                                                                              [
                                                                                {
                                                                                  {
                                                                                    Tuple2_match
                                                                                    k
                                                                                  }
                                                                                  r
                                                                                }
                                                                                ds
                                                                              ]
                                                                              [[These v] r]
                                                                            }
                                                                            (lam
                                                                              c
                                                                              k
                                                                              (lam
                                                                                i
                                                                                r
                                                                                [
                                                                                  [
                                                                                    [
                                                                                      {
                                                                                        [
                                                                                          Bool_match
                                                                                          [
                                                                                            [
                                                                                              dEq
                                                                                              c
                                                                                            ]
                                                                                            c
                                                                                          ]
                                                                                        ]
                                                                                        (fun Unit [[These v] r])
                                                                                      }
                                                                                      (lam
                                                                                        thunk
                                                                                        Unit
                                                                                        [
                                                                                          [
                                                                                            {
                                                                                              {
                                                                                                These
                                                                                                v
                                                                                              }
                                                                                              r
                                                                                            }
                                                                                            i
                                                                                          ]
                                                                                          i
                                                                                        ]
                                                                                      )
                                                                                    ]
                                                                                    (lam
                                                                                      thunk
                                                                                      Unit
                                                                                      [
                                                                                        go
                                                                                        xs
                                                                                      ]
                                                                                    )
                                                                                  ]
                                                                                  Unit
                                                                                ]
                                                                              )
                                                                            )
                                                                          ]
                                                                        )
                                                                      )
                                                                    )
                                                                  ]
                                                                  Unit
                                                                ]
                                                              )
                                                            )
                                                            [ go ds ]
                                                          )
                                                        ]
                                                      )
                                                    )
                                                  ]
                                                )
                                              ]
                                              ds
                                            ]
                                          ]
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                            (let
                              (nonrec)
                              (termbind
                                (strict)
                                (vardecl
                                  unionVal
                                  (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[These (con integer)] (con integer)]]]))
                                )
                                (lam
                                  ds
                                  [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                  (lam
                                    ds
                                    [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                    (let
                                      (rec)
                                      (termbind
                                        (strict)
                                        (vardecl
                                          go
                                          (fun [List [[Tuple2 (con bytestring)] [[These [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]]] [List [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[These (con integer)] (con integer)]]]])
                                        )
                                        (lam
                                          ds
                                          [List [[Tuple2 (con bytestring)] [[These [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]]]
                                          [
                                            [
                                              [
                                                {
                                                  [
                                                    {
                                                      Nil_match
                                                      [[Tuple2 (con bytestring)] [[These [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]]
                                                    }
                                                    ds
                                                  ]
                                                  (fun Unit [List [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[These (con integer)] (con integer)]]]])
                                                }
                                                (lam
                                                  thunk
                                                  Unit
                                                  {
                                                    Nil
                                                    [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[These (con integer)] (con integer)]]]
                                                  }
                                                )
                                              ]
                                              (lam
                                                ds
                                                [[Tuple2 (con bytestring)] [[These [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]]
                                                (lam
                                                  xs
                                                  [List [[Tuple2 (con bytestring)] [[These [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]]]
                                                  (lam
                                                    thunk
                                                    Unit
                                                    [
                                                      {
                                                        [
                                                          {
                                                            {
                                                              Tuple2_match
                                                              (con bytestring)
                                                            }
                                                            [[These [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                          }
                                                          ds
                                                        ]
                                                        [List [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[These (con integer)] (con integer)]]]]
                                                      }
                                                      (lam
                                                        c
                                                        (con bytestring)
                                                        (lam
                                                          i
                                                          [[These [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                          [
                                                            [
                                                              {
                                                                Cons
                                                                [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[These (con integer)] (con integer)]]]
                                                              }
                                                              [
                                                                [
                                                                  {
                                                                    {
                                                                      Tuple2
                                                                      (con bytestring)
                                                                    }
                                                                    [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[These (con integer)] (con integer)]]
                                                                  }
                                                                  c
                                                                ]
                                                                [
                                                                  [
                                                                    [
                                                                      {
                                                                        [
                                                                          {
                                                                            {
                                                                              These_match
                                                                              [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]
                                                                            }
                                                                            [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]
                                                                          }
                                                                          i
                                                                        ]
                                                                        [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[These (con integer)] (con integer)]]
                                                                      }
                                                                      (lam
                                                                        b
                                                                        [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]
                                                                        (let
                                                                          (rec)
                                                                          (termbind
                                                                            (strict
                                                                            )
                                                                            (vardecl
                                                                              go
                                                                              (fun [List [[Tuple2 (con bytestring)] (con integer)]] [List [[Tuple2 (con bytestring)] [[These (con integer)] (con integer)]]])
                                                                            )
                                                                            (lam
                                                                              ds
                                                                              [List [[Tuple2 (con bytestring)] (con integer)]]
                                                                              [
                                                                                [
                                                                                  [
                                                                                    {
                                                                                      [
                                                                                        {
                                                                                          Nil_match
                                                                                          [[Tuple2 (con bytestring)] (con integer)]
                                                                                        }
                                                                                        ds
                                                                                      ]
                                                                                      (fun Unit [List [[Tuple2 (con bytestring)] [[These (con integer)] (con integer)]]])
                                                                                    }
                                                                                    (lam
                                                                                      thunk
                                                                                      Unit
                                                                                      {
                                                                                        Nil
                                                                                        [[Tuple2 (con bytestring)] [[These (con integer)] (con integer)]]
                                                                                      }
                                                                                    )
                                                                                  ]
                                                                                  (lam
                                                                                    ds
                                                                                    [[Tuple2 (con bytestring)] (con integer)]
                                                                                    (lam
                                                                                      xs
                                                                                      [List [[Tuple2 (con bytestring)] (con integer)]]
                                                                                      (lam
                                                                                        thunk
                                                                                        Unit
                                                                                        [
                                                                                          {
                                                                                            [
                                                                                              {
                                                                                                {
                                                                                                  Tuple2_match
                                                                                                  (con bytestring)
                                                                                                }
                                                                                                (con integer)
                                                                                              }
                                                                                              ds
                                                                                            ]
                                                                                            [List [[Tuple2 (con bytestring)] [[These (con integer)] (con integer)]]]
                                                                                          }
                                                                                          (lam
                                                                                            c
                                                                                            (con bytestring)
                                                                                            (lam
                                                                                              i
                                                                                              (con integer)
                                                                                              [
                                                                                                [
                                                                                                  {
                                                                                                    Cons
                                                                                                    [[Tuple2 (con bytestring)] [[These (con integer)] (con integer)]]
                                                                                                  }
                                                                                                  [
                                                                                                    [
                                                                                                      {
                                                                                                        {
                                                                                                          Tuple2
                                                                                                          (con bytestring)
                                                                                                        }
                                                                                                        [[These (con integer)] (con integer)]
                                                                                                      }
                                                                                                      c
                                                                                                    ]
                                                                                                    [
                                                                                                      {
                                                                                                        {
                                                                                                          That
                                                                                                          (con integer)
                                                                                                        }
                                                                                                        (con integer)
                                                                                                      }
                                                                                                      i
                                                                                                    ]
                                                                                                  ]
                                                                                                ]
                                                                                                [
                                                                                                  go
                                                                                                  xs
                                                                                                ]
                                                                                              ]
                                                                                            )
                                                                                          )
                                                                                        ]
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                ]
                                                                                Unit
                                                                              ]
                                                                            )
                                                                          )
                                                                          [
                                                                            go b
                                                                          ]
                                                                        )
                                                                      )
                                                                    ]
                                                                    (lam
                                                                      a
                                                                      [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]
                                                                      (lam
                                                                        b
                                                                        [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]
                                                                        [
                                                                          [
                                                                            [
                                                                              {
                                                                                {
                                                                                  {
                                                                                    union
                                                                                    (con bytestring)
                                                                                  }
                                                                                  (con integer)
                                                                                }
                                                                                (con integer)
                                                                              }
                                                                              equalsByteString
                                                                            ]
                                                                            a
                                                                          ]
                                                                          b
                                                                        ]
                                                                      )
                                                                    )
                                                                  ]
                                                                  (lam
                                                                    a
                                                                    [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]
                                                                    (let
                                                                      (rec)
                                                                      (termbind
                                                                        (strict)
                                                                        (vardecl
                                                                          go
                                                                          (fun [List [[Tuple2 (con bytestring)] (con integer)]] [List [[Tuple2 (con bytestring)] [[These (con integer)] (con integer)]]])
                                                                        )
                                                                        (lam
                                                                          ds
                                                                          [List [[Tuple2 (con bytestring)] (con integer)]]
                                                                          [
                                                                            [
                                                                              [
                                                                                {
                                                                                  [
                                                                                    {
                                                                                      Nil_match
                                                                                      [[Tuple2 (con bytestring)] (con integer)]
                                                                                    }
                                                                                    ds
                                                                                  ]
                                                                                  (fun Unit [List [[Tuple2 (con bytestring)] [[These (con integer)] (con integer)]]])
                                                                                }
                                                                                (lam
                                                                                  thunk
                                                                                  Unit
                                                                                  {
                                                                                    Nil
                                                                                    [[Tuple2 (con bytestring)] [[These (con integer)] (con integer)]]
                                                                                  }
                                                                                )
                                                                              ]
                                                                              (lam
                                                                                ds
                                                                                [[Tuple2 (con bytestring)] (con integer)]
                                                                                (lam
                                                                                  xs
                                                                                  [List [[Tuple2 (con bytestring)] (con integer)]]
                                                                                  (lam
                                                                                    thunk
                                                                                    Unit
                                                                                    [
                                                                                      {
                                                                                        [
                                                                                          {
                                                                                            {
                                                                                              Tuple2_match
                                                                                              (con bytestring)
                                                                                            }
                                                                                            (con integer)
                                                                                          }
                                                                                          ds
                                                                                        ]
                                                                                        [List [[Tuple2 (con bytestring)] [[These (con integer)] (con integer)]]]
                                                                                      }
                                                                                      (lam
                                                                                        c
                                                                                        (con bytestring)
                                                                                        (lam
                                                                                          i
                                                                                          (con integer)
                                                                                          [
                                                                                            [
                                                                                              {
                                                                                                Cons
                                                                                                [[Tuple2 (con bytestring)] [[These (con integer)] (con integer)]]
                                                                                              }
                                                                                              [
                                                                                                [
                                                                                                  {
                                                                                                    {
                                                                                                      Tuple2
                                                                                                      (con bytestring)
                                                                                                    }
                                                                                                    [[These (con integer)] (con integer)]
                                                                                                  }
                                                                                                  c
                                                                                                ]
                                                                                                [
                                                                                                  {
                                                                                                    {
                                                                                                      This
                                                                                                      (con integer)
                                                                                                    }
                                                                                                    (con integer)
                                                                                                  }
                                                                                                  i
                                                                                                ]
                                                                                              ]
                                                                                            ]
                                                                                            [
                                                                                              go
                                                                                              xs
                                                                                            ]
                                                                                          ]
                                                                                        )
                                                                                      )
                                                                                    ]
                                                                                  )
                                                                                )
                                                                              )
                                                                            ]
                                                                            Unit
                                                                          ]
                                                                        )
                                                                      )
                                                                      [ go a ]
                                                                    )
                                                                  )
                                                                ]
                                                              ]
                                                            ]
                                                            [ go xs ]
                                                          ]
                                                        )
                                                      )
                                                    ]
                                                  )
                                                )
                                              )
                                            ]
                                            Unit
                                          ]
                                        )
                                      )
                                      [
                                        go
                                        [
                                          [
                                            [
                                              {
                                                {
                                                  { union (con bytestring) }
                                                  [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]
                                                }
                                                [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]
                                              }
                                              equalsByteString
                                            ]
                                            ds
                                          ]
                                          ds
                                        ]
                                      ]
                                    )
                                  )
                                )
                              )
                              (let
                                (nonrec)
                                (termbind
                                  (strict)
                                  (vardecl
                                    unionWith
                                    (fun (fun (con integer) (fun (con integer) (con integer))) (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]])))
                                  )
                                  (lam
                                    f
                                    (fun (con integer) (fun (con integer) (con integer)))
                                    (lam
                                      ls
                                      [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                      (lam
                                        rs
                                        [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                        (let
                                          (rec)
                                          (termbind
                                            (strict)
                                            (vardecl
                                              go
                                              (fun [List [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[These (con integer)] (con integer)]]]] [List [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]])
                                            )
                                            (lam
                                              ds
                                              [List [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[These (con integer)] (con integer)]]]]
                                              [
                                                [
                                                  [
                                                    {
                                                      [
                                                        {
                                                          Nil_match
                                                          [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[These (con integer)] (con integer)]]]
                                                        }
                                                        ds
                                                      ]
                                                      (fun Unit [List [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]])
                                                    }
                                                    (lam
                                                      thunk
                                                      Unit
                                                      {
                                                        Nil
                                                        [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                      }
                                                    )
                                                  ]
                                                  (lam
                                                    ds
                                                    [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[These (con integer)] (con integer)]]]
                                                    (lam
                                                      xs
                                                      [List [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[These (con integer)] (con integer)]]]]
                                                      (lam
                                                        thunk
                                                        Unit
                                                        [
                                                          {
                                                            [
                                                              {
                                                                {
                                                                  Tuple2_match
                                                                  (con bytestring)
                                                                }
                                                                [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[These (con integer)] (con integer)]]
                                                              }
                                                              ds
                                                            ]
                                                            [List [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]]
                                                          }
                                                          (lam
                                                            c
                                                            (con bytestring)
                                                            (lam
                                                              i
                                                              [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[These (con integer)] (con integer)]]
                                                              [
                                                                [
                                                                  {
                                                                    Cons
                                                                    [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                  }
                                                                  [
                                                                    [
                                                                      {
                                                                        {
                                                                          Tuple2
                                                                          (con bytestring)
                                                                        }
                                                                        [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]
                                                                      }
                                                                      c
                                                                    ]
                                                                    (let
                                                                      (rec)
                                                                      (termbind
                                                                        (strict)
                                                                        (vardecl
                                                                          go
                                                                          (fun [List [[Tuple2 (con bytestring)] [[These (con integer)] (con integer)]]] [List [[Tuple2 (con bytestring)] (con integer)]])
                                                                        )
                                                                        (lam
                                                                          ds
                                                                          [List [[Tuple2 (con bytestring)] [[These (con integer)] (con integer)]]]
                                                                          [
                                                                            [
                                                                              [
                                                                                {
                                                                                  [
                                                                                    {
                                                                                      Nil_match
                                                                                      [[Tuple2 (con bytestring)] [[These (con integer)] (con integer)]]
                                                                                    }
                                                                                    ds
                                                                                  ]
                                                                                  (fun Unit [List [[Tuple2 (con bytestring)] (con integer)]])
                                                                                }
                                                                                (lam
                                                                                  thunk
                                                                                  Unit
                                                                                  {
                                                                                    Nil
                                                                                    [[Tuple2 (con bytestring)] (con integer)]
                                                                                  }
                                                                                )
                                                                              ]
                                                                              (lam
                                                                                ds
                                                                                [[Tuple2 (con bytestring)] [[These (con integer)] (con integer)]]
                                                                                (lam
                                                                                  xs
                                                                                  [List [[Tuple2 (con bytestring)] [[These (con integer)] (con integer)]]]
                                                                                  (lam
                                                                                    thunk
                                                                                    Unit
                                                                                    [
                                                                                      {
                                                                                        [
                                                                                          {
                                                                                            {
                                                                                              Tuple2_match
                                                                                              (con bytestring)
                                                                                            }
                                                                                            [[These (con integer)] (con integer)]
                                                                                          }
                                                                                          ds
                                                                                        ]
                                                                                        [List [[Tuple2 (con bytestring)] (con integer)]]
                                                                                      }
                                                                                      (lam
                                                                                        c
                                                                                        (con bytestring)
                                                                                        (lam
                                                                                          i
                                                                                          [[These (con integer)] (con integer)]
                                                                                          [
                                                                                            [
                                                                                              {
                                                                                                Cons
                                                                                                [[Tuple2 (con bytestring)] (con integer)]
                                                                                              }
                                                                                              [
                                                                                                [
                                                                                                  {
                                                                                                    {
                                                                                                      Tuple2
                                                                                                      (con bytestring)
                                                                                                    }
                                                                                                    (con integer)
                                                                                                  }
                                                                                                  c
                                                                                                ]
                                                                                                [
                                                                                                  [
                                                                                                    [
                                                                                                      {
                                                                                                        [
                                                                                                          {
                                                                                                            {
                                                                                                              These_match
                                                                                                              (con integer)
                                                                                                            }
                                                                                                            (con integer)
                                                                                                          }
                                                                                                          i
                                                                                                        ]
                                                                                                        (con integer)
                                                                                                      }
                                                                                                      (lam
                                                                                                        b
                                                                                                        (con integer)
                                                                                                        [
                                                                                                          [
                                                                                                            f
                                                                                                            (con
                                                                                                              0
                                                                                                            )
                                                                                                          ]
                                                                                                          b
                                                                                                        ]
                                                                                                      )
                                                                                                    ]
                                                                                                    (lam
                                                                                                      a
                                                                                                      (con integer)
                                                                                                      (lam
                                                                                                        b
                                                                                                        (con integer)
                                                                                                        [
                                                                                                          [
                                                                                                            f
                                                                                                            a
                                                                                                          ]
                                                                                                          b
                                                                                                        ]
                                                                                                      )
                                                                                                    )
                                                                                                  ]
                                                                                                  (lam
                                                                                                    a
                                                                                                    (con integer)
                                                                                                    [
                                                                                                      [
                                                                                                        f
                                                                                                        a
                                                                                                      ]
                                                                                                      (con
                                                                                                        0
                                                                                                      )
                                                                                                    ]
                                                                                                  )
                                                                                                ]
                                                                                              ]
                                                                                            ]
                                                                                            [
                                                                                              go
                                                                                              xs
                                                                                            ]
                                                                                          ]
                                                                                        )
                                                                                      )
                                                                                    ]
                                                                                  )
                                                                                )
                                                                              )
                                                                            ]
                                                                            Unit
                                                                          ]
                                                                        )
                                                                      )
                                                                      [ go i ]
                                                                    )
                                                                  ]
                                                                ]
                                                                [ go xs ]
                                                              ]
                                                            )
                                                          )
                                                        ]
                                                      )
                                                    )
                                                  )
                                                ]
                                                Unit
                                              ]
                                            )
                                          )
                                          [ go [ [ unionVal ls ] rs ] ]
                                        )
                                      )
                                    )
                                  )
                                )
                                (let
                                  (nonrec)
                                  (termbind
                                    (strict)
                                    (vardecl
                                      fAdditiveGroupValue
                                      (fun [(lam a (type) a) [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]] (fun [(lam a (type) a) [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]))
                                    )
                                    (lam
                                      ds
                                      [(lam a (type) a) [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]]
                                      (lam
                                        ds
                                        [(lam a (type) a) [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]]
                                        [
                                          [ [ unionWith addInteger ] ds ]
                                          [
                                            [
                                              fAdditiveGroupValue_cscale
                                              fAdditiveGroupValue
                                            ]
                                            ds
                                          ]
                                        ]
                                      )
                                    )
                                  )
                                  (let
                                    (nonrec)
                                    (termbind
                                      (strict)
                                      (vardecl
                                        fAdditiveMonoidValue
                                        (fun [(lam a (type) a) [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]] (fun [(lam a (type) a) [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]))
                                      )
                                      (lam
                                        ds
                                        [(lam a (type) a) [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]]
                                        (lam
                                          ds
                                          [(lam a (type) a) [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]]
                                          [ [ [ unionWith addInteger ] ds ] ds ]
                                        )
                                      )
                                    )
                                    (let
                                      (nonrec)
                                      (datatypebind
                                        (datatype
                                          (tyvardecl
                                            Tuple3
                                            (fun (type) (fun (type) (fun (type) (type))))
                                          )
                                          (tyvardecl a (type))
                                          (tyvardecl b (type))
                                          (tyvardecl c (type))
                                          Tuple3_match
                                          (vardecl
                                            Tuple3
                                            (fun a (fun b (fun c [[[Tuple3 a] b] c])))
                                          )
                                        )
                                      )
                                      (let
                                        (nonrec)
                                        (datatypebind
                                          (datatype
                                            (tyvardecl Address (type))
                                            
                                            Address_match
                                            (vardecl
                                              PubKeyAddress
                                              (fun (con bytestring) Address)
                                            )
                                            (vardecl
                                              ScriptAddress
                                              (fun (con bytestring) Address)
                                            )
                                          )
                                        )
                                        (let
                                          (rec)
                                          (datatypebind
                                            (datatype
                                              (tyvardecl Data (type))
                                              
                                              Data_match
                                              (vardecl
                                                B (fun (con bytestring) Data)
                                              )
                                              (vardecl
                                                Constr
                                                (fun (con integer) (fun [List Data] Data))
                                              )
                                              (vardecl
                                                I (fun (con integer) Data)
                                              )
                                              (vardecl
                                                List (fun [List Data] Data)
                                              )
                                              (vardecl
                                                Map
                                                (fun [List [[Tuple2 Data] Data]] Data)
                                              )
                                            )
                                          )
                                          (let
                                            (nonrec)
                                            (datatypebind
                                              (datatype
                                                (tyvardecl
                                                  Extended (fun (type) (type))
                                                )
                                                (tyvardecl a (type))
                                                Extended_match
                                                (vardecl
                                                  Finite (fun a [Extended a])
                                                )
                                                (vardecl NegInf [Extended a])
                                                (vardecl PosInf [Extended a])
                                              )
                                            )
                                            (let
                                              (nonrec)
                                              (datatypebind
                                                (datatype
                                                  (tyvardecl Future (type))
                                                  
                                                  Future_match
                                                  (vardecl
                                                    Future
                                                    (fun (con integer) (fun (con integer) (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] (fun (con bytestring) (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] Future))))))
                                                  )
                                                )
                                              )
                                              (let
                                                (nonrec)
                                                (datatypebind
                                                  (datatype
                                                    (tyvardecl
                                                      FutureAccounts (type)
                                                    )
                                                    
                                                    FutureAccounts_match
                                                    (vardecl
                                                      FutureAccounts
                                                      (fun [[Tuple2 (con bytestring)] (con bytestring)] (fun (con bytestring) (fun [[Tuple2 (con bytestring)] (con bytestring)] (fun (con bytestring) FutureAccounts))))
                                                    )
                                                  )
                                                )
                                                (let
                                                  (nonrec)
                                                  (datatypebind
                                                    (datatype
                                                      (tyvardecl
                                                        OracleValue
                                                        (fun (type) (type))
                                                      )
                                                      (tyvardecl a (type))
                                                      OracleValue_match
                                                      (vardecl
                                                        OracleValue
                                                        (fun (con bytestring) (fun (con integer) (fun a [OracleValue a])))
                                                      )
                                                    )
                                                  )
                                                  (let
                                                    (nonrec)
                                                    (datatypebind
                                                      (datatype
                                                        (tyvardecl Role (type))
                                                        
                                                        Role_match
                                                        (vardecl Long Role)
                                                        (vardecl Short Role)
                                                      )
                                                    )
                                                    (let
                                                      (nonrec)
                                                      (datatypebind
                                                        (datatype
                                                          (tyvardecl
                                                            FutureAction (type)
                                                          )
                                                          
                                                          FutureAction_match
                                                          (vardecl
                                                            AdjustMargin
                                                            (fun Role (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] FutureAction))
                                                          )
                                                          (vardecl
                                                            Settle
                                                            (fun [OracleValue [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]] FutureAction)
                                                          )
                                                          (vardecl
                                                            SettleEarly
                                                            (fun [OracleValue [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]] FutureAction)
                                                          )
                                                        )
                                                      )
                                                      (let
                                                        (nonrec)
                                                        (datatypebind
                                                          (datatype
                                                            (tyvardecl
                                                              Margins (type)
                                                            )
                                                            
                                                            Margins_match
                                                            (vardecl
                                                              Margins
                                                              (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] Margins))
                                                            )
                                                          )
                                                        )
                                                        (let
                                                          (nonrec)
                                                          (datatypebind
                                                            (datatype
                                                              (tyvardecl
                                                                FutureState
                                                                (type)
                                                              )
                                                              
                                                              FutureState_match
                                                              (vardecl
                                                                Finished
                                                                FutureState
                                                              )
                                                              (vardecl
                                                                Running
                                                                (fun Margins FutureState)
                                                              )
                                                            )
                                                          )
                                                          (let
                                                            (nonrec)
                                                            (datatypebind
                                                              (datatype
                                                                (tyvardecl
                                                                  LowerBound
                                                                  (fun (type) (type))
                                                                )
                                                                (tyvardecl
                                                                  a (type)
                                                                )
                                                                LowerBound_match
                                                                (vardecl
                                                                  LowerBound
                                                                  (fun [Extended a] (fun Bool [LowerBound a]))
                                                                )
                                                              )
                                                            )
                                                            (let
                                                              (nonrec)
                                                              (datatypebind
                                                                (datatype
                                                                  (tyvardecl
                                                                    UpperBound
                                                                    (fun (type) (type))
                                                                  )
                                                                  (tyvardecl
                                                                    a (type)
                                                                  )
                                                                  UpperBound_match
                                                                  (vardecl
                                                                    UpperBound
                                                                    (fun [Extended a] (fun Bool [UpperBound a]))
                                                                  )
                                                                )
                                                              )
                                                              (let
                                                                (nonrec)
                                                                (datatypebind
                                                                  (datatype
                                                                    (tyvardecl
                                                                      Interval
                                                                      (fun (type) (type))
                                                                    )
                                                                    (tyvardecl
                                                                      a (type)
                                                                    )
                                                                    Interval_match
                                                                    (vardecl
                                                                      Interval
                                                                      (fun [LowerBound a] (fun [UpperBound a] [Interval a]))
                                                                    )
                                                                  )
                                                                )
                                                                (let
                                                                  (nonrec)
                                                                  (datatypebind
                                                                    (datatype
                                                                      (tyvardecl
                                                                        Maybe
                                                                        (fun (type) (type))
                                                                      )
                                                                      (tyvardecl
                                                                        a (type)
                                                                      )
                                                                      Maybe_match
                                                                      (vardecl
                                                                        Just
                                                                        (fun a [Maybe a])
                                                                      )
                                                                      (vardecl
                                                                        Nothing
                                                                        [Maybe a]
                                                                      )
                                                                    )
                                                                  )
                                                                  (let
                                                                    (nonrec)
                                                                    (datatypebind
                                                                      (datatype
                                                                        (tyvardecl
                                                                          TxOutRef
                                                                          (type)
                                                                        )
                                                                        
                                                                        TxOutRef_match
                                                                        (vardecl
                                                                          TxOutRef
                                                                          (fun (con bytestring) (fun (con integer) TxOutRef))
                                                                        )
                                                                      )
                                                                    )
                                                                    (let
                                                                      (nonrec)
                                                                      (datatypebind
                                                                        (datatype
                                                                          (tyvardecl
                                                                            TxConstraint
                                                                            (type)
                                                                          )
                                                                          
                                                                          TxConstraint_match
                                                                          (vardecl
                                                                            MustBeSignedBy
                                                                            (fun (con bytestring) TxConstraint)
                                                                          )
                                                                          (vardecl
                                                                            MustForgeValue
                                                                            (fun (con bytestring) (fun (con bytestring) (fun (con integer) TxConstraint)))
                                                                          )
                                                                          (vardecl
                                                                            MustIncludeDataValue
                                                                            (fun Data TxConstraint)
                                                                          )
                                                                          (vardecl
                                                                            MustPayToOtherScript
                                                                            (fun (con bytestring) (fun Data (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] TxConstraint)))
                                                                          )
                                                                          (vardecl
                                                                            MustPayToPubKey
                                                                            (fun (con bytestring) (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] TxConstraint))
                                                                          )
                                                                          (vardecl
                                                                            MustSpendPubKeyOutput
                                                                            (fun TxOutRef TxConstraint)
                                                                          )
                                                                          (vardecl
                                                                            MustSpendScriptOutput
                                                                            (fun TxOutRef (fun Data TxConstraint))
                                                                          )
                                                                          (vardecl
                                                                            MustSpendValue
                                                                            (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] TxConstraint)
                                                                          )
                                                                          (vardecl
                                                                            MustValidateIn
                                                                            (fun [Interval (con integer)] TxConstraint)
                                                                          )
                                                                        )
                                                                      )
                                                                      (let
                                                                        (nonrec)
                                                                        (datatypebind
                                                                          (datatype
                                                                            (tyvardecl
                                                                              NewState
                                                                              (fun (type) (type))
                                                                            )
                                                                            (tyvardecl
                                                                              s
                                                                              (type)
                                                                            )
                                                                            NewState_match
                                                                            (vardecl
                                                                              NewState
                                                                              (fun s (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] (fun [List TxConstraint] [NewState s])))
                                                                            )
                                                                          )
                                                                        )
                                                                        (let
                                                                          (nonrec
                                                                          )
                                                                          (datatypebind
                                                                            (datatype
                                                                              (tyvardecl
                                                                                OldState
                                                                                (fun (type) (type))
                                                                              )
                                                                              (tyvardecl
                                                                                s
                                                                                (type)
                                                                              )
                                                                              OldState_match
                                                                              (vardecl
                                                                                OldState
                                                                                (fun s (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] [OldState s]))
                                                                              )
                                                                            )
                                                                          )
                                                                          (let
                                                                            (nonrec
                                                                            )
                                                                            (datatypebind
                                                                              (datatype
                                                                                (tyvardecl
                                                                                  Payouts
                                                                                  (type)
                                                                                )
                                                                                
                                                                                Payouts_match
                                                                                (vardecl
                                                                                  Payouts
                                                                                  (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] Payouts))
                                                                                )
                                                                              )
                                                                            )
                                                                            (let
                                                                              (nonrec
                                                                              )
                                                                              (datatypebind
                                                                                (datatype
                                                                                  (tyvardecl
                                                                                    PendingTxIn
                                                                                    (fun (type) (type))
                                                                                  )
                                                                                  (tyvardecl
                                                                                    w
                                                                                    (type)
                                                                                  )
                                                                                  PendingTxIn_match
                                                                                  (vardecl
                                                                                    PendingTxIn
                                                                                    (fun TxOutRef (fun w (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] [PendingTxIn w])))
                                                                                  )
                                                                                )
                                                                              )
                                                                              (let
                                                                                (nonrec
                                                                                )
                                                                                (datatypebind
                                                                                  (datatype
                                                                                    (tyvardecl
                                                                                      TxOutType
                                                                                      (type)
                                                                                    )
                                                                                    
                                                                                    TxOutType_match
                                                                                    (vardecl
                                                                                      PayToPubKey
                                                                                      TxOutType
                                                                                    )
                                                                                    (vardecl
                                                                                      PayToScript
                                                                                      (fun (con bytestring) TxOutType)
                                                                                    )
                                                                                  )
                                                                                )
                                                                                (let
                                                                                  (nonrec
                                                                                  )
                                                                                  (datatypebind
                                                                                    (datatype
                                                                                      (tyvardecl
                                                                                        TxOut
                                                                                        (type)
                                                                                      )
                                                                                      
                                                                                      TxOut_match
                                                                                      (vardecl
                                                                                        TxOut
                                                                                        (fun Address (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] (fun TxOutType TxOut)))
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                  (let
                                                                                    (nonrec
                                                                                    )
                                                                                    (datatypebind
                                                                                      (datatype
                                                                                        (tyvardecl
                                                                                          PendingTx
                                                                                          (fun (type) (type))
                                                                                        )
                                                                                        (tyvardecl
                                                                                          i
                                                                                          (type)
                                                                                        )
                                                                                        PendingTx_match
                                                                                        (vardecl
                                                                                          PendingTx
                                                                                          (fun [List [PendingTxIn [Maybe [[[Tuple3 (con bytestring)] (con bytestring)] (con bytestring)]]]] (fun [List TxOut] (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] (fun i (fun [Interval (con integer)] (fun [List (con bytestring)] (fun [List (con bytestring)] (fun [List [[Tuple2 (con bytestring)] Data]] (fun (con bytestring) [PendingTx i]))))))))))
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                    (let
                                                                                      (nonrec
                                                                                      )
                                                                                      (datatypebind
                                                                                        (datatype
                                                                                          (tyvardecl
                                                                                            StateMachine
                                                                                            (fun (type) (fun (type) (type)))
                                                                                          )
                                                                                          (tyvardecl
                                                                                            s
                                                                                            (type)
                                                                                          )
                                                                                          (tyvardecl
                                                                                            i
                                                                                            (type)
                                                                                          )
                                                                                          StateMachine_match
                                                                                          (vardecl
                                                                                            StateMachine
                                                                                            (fun (fun [OldState s] (fun i [Maybe [NewState s]])) (fun (fun s Bool) (fun (fun s (fun i (fun [PendingTx [PendingTxIn [[[Tuple3 (con bytestring)] (con bytestring)] (con bytestring)]]] Bool))) [[StateMachine s] i])))
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                      (let
                                                                                        (nonrec
                                                                                        )
                                                                                        (termbind
                                                                                          (strict
                                                                                          )
                                                                                          (vardecl
                                                                                            adjustMargin
                                                                                            (fun Role (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] (fun Margins Margins)))
                                                                                          )
                                                                                          (lam
                                                                                            role
                                                                                            Role
                                                                                            (lam
                                                                                              value
                                                                                              [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                              (lam
                                                                                                accounts
                                                                                                Margins
                                                                                                [
                                                                                                  [
                                                                                                    [
                                                                                                      {
                                                                                                        [
                                                                                                          Role_match
                                                                                                          role
                                                                                                        ]
                                                                                                        (fun Unit Margins)
                                                                                                      }
                                                                                                      (lam
                                                                                                        thunk
                                                                                                        Unit
                                                                                                        [
                                                                                                          {
                                                                                                            [
                                                                                                              Margins_match
                                                                                                              accounts
                                                                                                            ]
                                                                                                            Margins
                                                                                                          }
                                                                                                          (lam
                                                                                                            ds
                                                                                                            [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                            (lam
                                                                                                              ds
                                                                                                              [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                              [
                                                                                                                [
                                                                                                                  Margins
                                                                                                                  ds
                                                                                                                ]
                                                                                                                [
                                                                                                                  [
                                                                                                                    [
                                                                                                                      unionWith
                                                                                                                      addInteger
                                                                                                                    ]
                                                                                                                    ds
                                                                                                                  ]
                                                                                                                  value
                                                                                                                ]
                                                                                                              ]
                                                                                                            )
                                                                                                          )
                                                                                                        ]
                                                                                                      )
                                                                                                    ]
                                                                                                    (lam
                                                                                                      thunk
                                                                                                      Unit
                                                                                                      [
                                                                                                        {
                                                                                                          [
                                                                                                            Margins_match
                                                                                                            accounts
                                                                                                          ]
                                                                                                          Margins
                                                                                                        }
                                                                                                        (lam
                                                                                                          ds
                                                                                                          [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                          (lam
                                                                                                            ds
                                                                                                            [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                            [
                                                                                                              [
                                                                                                                Margins
                                                                                                                [
                                                                                                                  [
                                                                                                                    [
                                                                                                                      unionWith
                                                                                                                      addInteger
                                                                                                                    ]
                                                                                                                    ds
                                                                                                                  ]
                                                                                                                  value
                                                                                                                ]
                                                                                                              ]
                                                                                                              ds
                                                                                                            ]
                                                                                                          )
                                                                                                        )
                                                                                                      ]
                                                                                                    )
                                                                                                  ]
                                                                                                  Unit
                                                                                                ]
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                        (let
                                                                                          (nonrec
                                                                                          )
                                                                                          (termbind
                                                                                            (strict
                                                                                            )
                                                                                            (vardecl
                                                                                              build
                                                                                              (all a (type) (fun (all b (type) (fun (fun a (fun b b)) (fun b b))) [List a]))
                                                                                            )
                                                                                            (abs
                                                                                              a
                                                                                              (type)
                                                                                              (lam
                                                                                                g
                                                                                                (all b (type) (fun (fun a (fun b b)) (fun b b)))
                                                                                                [
                                                                                                  [
                                                                                                    {
                                                                                                      g
                                                                                                      [List a]
                                                                                                    }
                                                                                                    {
                                                                                                      Cons
                                                                                                      a
                                                                                                    }
                                                                                                  ]
                                                                                                  {
                                                                                                    Nil
                                                                                                    a
                                                                                                  }
                                                                                                ]
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                          (let
                                                                                            (nonrec
                                                                                            )
                                                                                            (termbind
                                                                                              (strict
                                                                                              )
                                                                                              (vardecl
                                                                                                checkBinRel
                                                                                                (fun (fun (con integer) (fun (con integer) Bool)) (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] Bool)))
                                                                                              )
                                                                                              (lam
                                                                                                f
                                                                                                (fun (con integer) (fun (con integer) Bool))
                                                                                                (lam
                                                                                                  l
                                                                                                  [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                  (lam
                                                                                                    r
                                                                                                    [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                    (let
                                                                                                      (rec
                                                                                                      )
                                                                                                      (termbind
                                                                                                        (strict
                                                                                                        )
                                                                                                        (vardecl
                                                                                                          go
                                                                                                          (fun [List [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[These (con integer)] (con integer)]]]] Bool)
                                                                                                        )
                                                                                                        (lam
                                                                                                          xs
                                                                                                          [List [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[These (con integer)] (con integer)]]]]
                                                                                                          [
                                                                                                            [
                                                                                                              [
                                                                                                                {
                                                                                                                  [
                                                                                                                    {
                                                                                                                      Nil_match
                                                                                                                      [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[These (con integer)] (con integer)]]]
                                                                                                                    }
                                                                                                                    xs
                                                                                                                  ]
                                                                                                                  (fun Unit Bool)
                                                                                                                }
                                                                                                                (lam
                                                                                                                  thunk
                                                                                                                  Unit
                                                                                                                  True
                                                                                                                )
                                                                                                              ]
                                                                                                              (lam
                                                                                                                ds
                                                                                                                [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[These (con integer)] (con integer)]]]
                                                                                                                (lam
                                                                                                                  xs
                                                                                                                  [List [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[These (con integer)] (con integer)]]]]
                                                                                                                  (lam
                                                                                                                    thunk
                                                                                                                    Unit
                                                                                                                    [
                                                                                                                      {
                                                                                                                        [
                                                                                                                          {
                                                                                                                            {
                                                                                                                              Tuple2_match
                                                                                                                              (con bytestring)
                                                                                                                            }
                                                                                                                            [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[These (con integer)] (con integer)]]
                                                                                                                          }
                                                                                                                          ds
                                                                                                                        ]
                                                                                                                        Bool
                                                                                                                      }
                                                                                                                      (lam
                                                                                                                        ds
                                                                                                                        (con bytestring)
                                                                                                                        (lam
                                                                                                                          x
                                                                                                                          [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[These (con integer)] (con integer)]]
                                                                                                                          (let
                                                                                                                            (rec
                                                                                                                            )
                                                                                                                            (termbind
                                                                                                                              (strict
                                                                                                                              )
                                                                                                                              (vardecl
                                                                                                                                go
                                                                                                                                (fun [List [[Tuple2 (con bytestring)] [[These (con integer)] (con integer)]]] Bool)
                                                                                                                              )
                                                                                                                              (lam
                                                                                                                                xs
                                                                                                                                [List [[Tuple2 (con bytestring)] [[These (con integer)] (con integer)]]]
                                                                                                                                [
                                                                                                                                  [
                                                                                                                                    [
                                                                                                                                      {
                                                                                                                                        [
                                                                                                                                          {
                                                                                                                                            Nil_match
                                                                                                                                            [[Tuple2 (con bytestring)] [[These (con integer)] (con integer)]]
                                                                                                                                          }
                                                                                                                                          xs
                                                                                                                                        ]
                                                                                                                                        (fun Unit Bool)
                                                                                                                                      }
                                                                                                                                      (lam
                                                                                                                                        thunk
                                                                                                                                        Unit
                                                                                                                                        [
                                                                                                                                          go
                                                                                                                                          xs
                                                                                                                                        ]
                                                                                                                                      )
                                                                                                                                    ]
                                                                                                                                    (lam
                                                                                                                                      ds
                                                                                                                                      [[Tuple2 (con bytestring)] [[These (con integer)] (con integer)]]
                                                                                                                                      (lam
                                                                                                                                        xs
                                                                                                                                        [List [[Tuple2 (con bytestring)] [[These (con integer)] (con integer)]]]
                                                                                                                                        (lam
                                                                                                                                          thunk
                                                                                                                                          Unit
                                                                                                                                          [
                                                                                                                                            {
                                                                                                                                              [
                                                                                                                                                {
                                                                                                                                                  {
                                                                                                                                                    Tuple2_match
                                                                                                                                                    (con bytestring)
                                                                                                                                                  }
                                                                                                                                                  [[These (con integer)] (con integer)]
                                                                                                                                                }
                                                                                                                                                ds
                                                                                                                                              ]
                                                                                                                                              Bool
                                                                                                                                            }
                                                                                                                                            (lam
                                                                                                                                              ds
                                                                                                                                              (con bytestring)
                                                                                                                                              (lam
                                                                                                                                                x
                                                                                                                                                [[These (con integer)] (con integer)]
                                                                                                                                                [
                                                                                                                                                  [
                                                                                                                                                    [
                                                                                                                                                      {
                                                                                                                                                        [
                                                                                                                                                          {
                                                                                                                                                            {
                                                                                                                                                              These_match
                                                                                                                                                              (con integer)
                                                                                                                                                            }
                                                                                                                                                            (con integer)
                                                                                                                                                          }
                                                                                                                                                          x
                                                                                                                                                        ]
                                                                                                                                                        Bool
                                                                                                                                                      }
                                                                                                                                                      (lam
                                                                                                                                                        b
                                                                                                                                                        (con integer)
                                                                                                                                                        [
                                                                                                                                                          [
                                                                                                                                                            [
                                                                                                                                                              {
                                                                                                                                                                [
                                                                                                                                                                  Bool_match
                                                                                                                                                                  [
                                                                                                                                                                    [
                                                                                                                                                                      f
                                                                                                                                                                      (con
                                                                                                                                                                        0
                                                                                                                                                                      )
                                                                                                                                                                    ]
                                                                                                                                                                    b
                                                                                                                                                                  ]
                                                                                                                                                                ]
                                                                                                                                                                (fun Unit Bool)
                                                                                                                                                              }
                                                                                                                                                              (lam
                                                                                                                                                                thunk
                                                                                                                                                                Unit
                                                                                                                                                                [
                                                                                                                                                                  go
                                                                                                                                                                  xs
                                                                                                                                                                ]
                                                                                                                                                              )
                                                                                                                                                            ]
                                                                                                                                                            (lam
                                                                                                                                                              thunk
                                                                                                                                                              Unit
                                                                                                                                                              False
                                                                                                                                                            )
                                                                                                                                                          ]
                                                                                                                                                          Unit
                                                                                                                                                        ]
                                                                                                                                                      )
                                                                                                                                                    ]
                                                                                                                                                    (lam
                                                                                                                                                      a
                                                                                                                                                      (con integer)
                                                                                                                                                      (lam
                                                                                                                                                        b
                                                                                                                                                        (con integer)
                                                                                                                                                        [
                                                                                                                                                          [
                                                                                                                                                            [
                                                                                                                                                              {
                                                                                                                                                                [
                                                                                                                                                                  Bool_match
                                                                                                                                                                  [
                                                                                                                                                                    [
                                                                                                                                                                      f
                                                                                                                                                                      a
                                                                                                                                                                    ]
                                                                                                                                                                    b
                                                                                                                                                                  ]
                                                                                                                                                                ]
                                                                                                                                                                (fun Unit Bool)
                                                                                                                                                              }
                                                                                                                                                              (lam
                                                                                                                                                                thunk
                                                                                                                                                                Unit
                                                                                                                                                                [
                                                                                                                                                                  go
                                                                                                                                                                  xs
                                                                                                                                                                ]
                                                                                                                                                              )
                                                                                                                                                            ]
                                                                                                                                                            (lam
                                                                                                                                                              thunk
                                                                                                                                                              Unit
                                                                                                                                                              False
                                                                                                                                                            )
                                                                                                                                                          ]
                                                                                                                                                          Unit
                                                                                                                                                        ]
                                                                                                                                                      )
                                                                                                                                                    )
                                                                                                                                                  ]
                                                                                                                                                  (lam
                                                                                                                                                    a
                                                                                                                                                    (con integer)
                                                                                                                                                    [
                                                                                                                                                      [
                                                                                                                                                        [
                                                                                                                                                          {
                                                                                                                                                            [
                                                                                                                                                              Bool_match
                                                                                                                                                              [
                                                                                                                                                                [
                                                                                                                                                                  f
                                                                                                                                                                  a
                                                                                                                                                                ]
                                                                                                                                                                (con
                                                                                                                                                                  0
                                                                                                                                                                )
                                                                                                                                                              ]
                                                                                                                                                            ]
                                                                                                                                                            (fun Unit Bool)
                                                                                                                                                          }
                                                                                                                                                          (lam
                                                                                                                                                            thunk
                                                                                                                                                            Unit
                                                                                                                                                            [
                                                                                                                                                              go
                                                                                                                                                              xs
                                                                                                                                                            ]
                                                                                                                                                          )
                                                                                                                                                        ]
                                                                                                                                                        (lam
                                                                                                                                                          thunk
                                                                                                                                                          Unit
                                                                                                                                                          False
                                                                                                                                                        )
                                                                                                                                                      ]
                                                                                                                                                      Unit
                                                                                                                                                    ]
                                                                                                                                                  )
                                                                                                                                                ]
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          ]
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  ]
                                                                                                                                  Unit
                                                                                                                                ]
                                                                                                                              )
                                                                                                                            )
                                                                                                                            [
                                                                                                                              go
                                                                                                                              x
                                                                                                                            ]
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    ]
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            ]
                                                                                                            Unit
                                                                                                          ]
                                                                                                        )
                                                                                                      )
                                                                                                      [
                                                                                                        go
                                                                                                        [
                                                                                                          [
                                                                                                            unionVal
                                                                                                            l
                                                                                                          ]
                                                                                                          r
                                                                                                        ]
                                                                                                      ]
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                            (let
                                                                                              (nonrec
                                                                                              )
                                                                                              (termbind
                                                                                                (strict
                                                                                                )
                                                                                                (vardecl
                                                                                                  equalsInteger
                                                                                                  (fun (con integer) (fun (con integer) Bool))
                                                                                                )
                                                                                                (lam
                                                                                                  arg
                                                                                                  (con integer)
                                                                                                  (lam
                                                                                                    arg
                                                                                                    (con integer)
                                                                                                    [
                                                                                                      (lam
                                                                                                        b
                                                                                                        (all a (type) (fun a (fun a a)))
                                                                                                        [
                                                                                                          [
                                                                                                            {
                                                                                                              b
                                                                                                              Bool
                                                                                                            }
                                                                                                            True
                                                                                                          ]
                                                                                                          False
                                                                                                        ]
                                                                                                      )
                                                                                                      [
                                                                                                        [
                                                                                                          (builtin
                                                                                                            equalsInteger
                                                                                                          )
                                                                                                          arg
                                                                                                        ]
                                                                                                        arg
                                                                                                      ]
                                                                                                    ]
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                              (let
                                                                                                (nonrec
                                                                                                )
                                                                                                (termbind
                                                                                                  (strict
                                                                                                  )
                                                                                                  (vardecl
                                                                                                    lowerBound
                                                                                                    (all a (type) (fun a [LowerBound a]))
                                                                                                  )
                                                                                                  (abs
                                                                                                    a
                                                                                                    (type)
                                                                                                    (lam
                                                                                                      a
                                                                                                      a
                                                                                                      [
                                                                                                        [
                                                                                                          {
                                                                                                            LowerBound
                                                                                                            a
                                                                                                          }
                                                                                                          [
                                                                                                            {
                                                                                                              Finite
                                                                                                              a
                                                                                                            }
                                                                                                            a
                                                                                                          ]
                                                                                                        ]
                                                                                                        True
                                                                                                      ]
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                (let
                                                                                                  (nonrec
                                                                                                  )
                                                                                                  (termbind
                                                                                                    (strict
                                                                                                    )
                                                                                                    (vardecl
                                                                                                      from
                                                                                                      (all a (type) (fun a [Interval a]))
                                                                                                    )
                                                                                                    (abs
                                                                                                      a
                                                                                                      (type)
                                                                                                      (lam
                                                                                                        s
                                                                                                        a
                                                                                                        [
                                                                                                          [
                                                                                                            {
                                                                                                              Interval
                                                                                                              a
                                                                                                            }
                                                                                                            [
                                                                                                              {
                                                                                                                lowerBound
                                                                                                                a
                                                                                                              }
                                                                                                              s
                                                                                                            ]
                                                                                                          ]
                                                                                                          [
                                                                                                            [
                                                                                                              {
                                                                                                                UpperBound
                                                                                                                a
                                                                                                              }
                                                                                                              {
                                                                                                                PosInf
                                                                                                                a
                                                                                                              }
                                                                                                            ]
                                                                                                            True
                                                                                                          ]
                                                                                                        ]
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                  (let
                                                                                                    (nonrec
                                                                                                    )
                                                                                                    (termbind
                                                                                                      (strict
                                                                                                      )
                                                                                                      (vardecl
                                                                                                        mkStateMachine
                                                                                                        (all s (type) (all i (type) (fun s (fun i (fun [PendingTx [PendingTxIn [[[Tuple3 (con bytestring)] (con bytestring)] (con bytestring)]]] Bool)))))
                                                                                                      )
                                                                                                      (abs
                                                                                                        s
                                                                                                        (type)
                                                                                                        (abs
                                                                                                          i
                                                                                                          (type)
                                                                                                          (lam
                                                                                                            ds
                                                                                                            s
                                                                                                            (lam
                                                                                                              ds
                                                                                                              i
                                                                                                              (lam
                                                                                                                ds
                                                                                                                [PendingTx [PendingTxIn [[[Tuple3 (con bytestring)] (con bytestring)] (con bytestring)]]]
                                                                                                                True
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                    (let
                                                                                                      (nonrec
                                                                                                      )
                                                                                                      (termbind
                                                                                                        (strict
                                                                                                        )
                                                                                                        (vardecl
                                                                                                          greaterThanInteger
                                                                                                          (fun (con integer) (fun (con integer) Bool))
                                                                                                        )
                                                                                                        (lam
                                                                                                          arg
                                                                                                          (con integer)
                                                                                                          (lam
                                                                                                            arg
                                                                                                            (con integer)
                                                                                                            [
                                                                                                              (lam
                                                                                                                b
                                                                                                                (all a (type) (fun a (fun a a)))
                                                                                                                [
                                                                                                                  [
                                                                                                                    {
                                                                                                                      b
                                                                                                                      Bool
                                                                                                                    }
                                                                                                                    True
                                                                                                                  ]
                                                                                                                  False
                                                                                                                ]
                                                                                                              )
                                                                                                              [
                                                                                                                [
                                                                                                                  (builtin
                                                                                                                    greaterThanInteger
                                                                                                                  )
                                                                                                                  arg
                                                                                                                ]
                                                                                                                arg
                                                                                                              ]
                                                                                                            ]
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                      (let
                                                                                                        (nonrec
                                                                                                        )
                                                                                                        (termbind
                                                                                                          (strict
                                                                                                          )
                                                                                                          (vardecl
                                                                                                            unitData
                                                                                                            (con integer)
                                                                                                          )
                                                                                                          (con
                                                                                                            0
                                                                                                          )
                                                                                                        )
                                                                                                        (let
                                                                                                          (nonrec
                                                                                                          )
                                                                                                          (termbind
                                                                                                            (nonstrict
                                                                                                            )
                                                                                                            (vardecl
                                                                                                              unitData
                                                                                                              Data
                                                                                                            )
                                                                                                            [
                                                                                                              [
                                                                                                                Constr
                                                                                                                unitData
                                                                                                              ]
                                                                                                              {
                                                                                                                Nil
                                                                                                                Data
                                                                                                              }
                                                                                                            ]
                                                                                                          )
                                                                                                          (let
                                                                                                            (nonrec
                                                                                                            )
                                                                                                            (termbind
                                                                                                              (strict
                                                                                                              )
                                                                                                              (vardecl
                                                                                                                payoutsTx
                                                                                                                (fun Payouts (fun FutureAccounts [List TxConstraint]))
                                                                                                              )
                                                                                                              (lam
                                                                                                                ds
                                                                                                                Payouts
                                                                                                                (lam
                                                                                                                  ds
                                                                                                                  FutureAccounts
                                                                                                                  [
                                                                                                                    {
                                                                                                                      [
                                                                                                                        Payouts_match
                                                                                                                        ds
                                                                                                                      ]
                                                                                                                      [List TxConstraint]
                                                                                                                    }
                                                                                                                    (lam
                                                                                                                      ds
                                                                                                                      [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                      (lam
                                                                                                                        ds
                                                                                                                        [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                        [
                                                                                                                          {
                                                                                                                            [
                                                                                                                              FutureAccounts_match
                                                                                                                              ds
                                                                                                                            ]
                                                                                                                            [List TxConstraint]
                                                                                                                          }
                                                                                                                          (lam
                                                                                                                            ds
                                                                                                                            [[Tuple2 (con bytestring)] (con bytestring)]
                                                                                                                            (lam
                                                                                                                              ds
                                                                                                                              (con bytestring)
                                                                                                                              (lam
                                                                                                                                ds
                                                                                                                                [[Tuple2 (con bytestring)] (con bytestring)]
                                                                                                                                (lam
                                                                                                                                  ds
                                                                                                                                  (con bytestring)
                                                                                                                                  [
                                                                                                                                    {
                                                                                                                                      build
                                                                                                                                      TxConstraint
                                                                                                                                    }
                                                                                                                                    (abs
                                                                                                                                      a
                                                                                                                                      (type)
                                                                                                                                      (lam
                                                                                                                                        c
                                                                                                                                        (fun TxConstraint (fun a a))
                                                                                                                                        (lam
                                                                                                                                          n
                                                                                                                                          a
                                                                                                                                          [
                                                                                                                                            [
                                                                                                                                              c
                                                                                                                                              [
                                                                                                                                                [
                                                                                                                                                  [
                                                                                                                                                    MustPayToOtherScript
                                                                                                                                                    ds
                                                                                                                                                  ]
                                                                                                                                                  unitData
                                                                                                                                                ]
                                                                                                                                                ds
                                                                                                                                              ]
                                                                                                                                            ]
                                                                                                                                            [
                                                                                                                                              [
                                                                                                                                                c
                                                                                                                                                [
                                                                                                                                                  [
                                                                                                                                                    [
                                                                                                                                                      MustPayToOtherScript
                                                                                                                                                      ds
                                                                                                                                                    ]
                                                                                                                                                    unitData
                                                                                                                                                  ]
                                                                                                                                                  ds
                                                                                                                                                ]
                                                                                                                                              ]
                                                                                                                                              n
                                                                                                                                            ]
                                                                                                                                          ]
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  ]
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                        ]
                                                                                                                      )
                                                                                                                    )
                                                                                                                  ]
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                            (let
                                                                                                              (nonrec
                                                                                                              )
                                                                                                              (termbind
                                                                                                                (strict
                                                                                                                )
                                                                                                                (vardecl
                                                                                                                  totalMargin
                                                                                                                  (fun Margins [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]])
                                                                                                                )
                                                                                                                (lam
                                                                                                                  ds
                                                                                                                  Margins
                                                                                                                  [
                                                                                                                    {
                                                                                                                      [
                                                                                                                        Margins_match
                                                                                                                        ds
                                                                                                                      ]
                                                                                                                      [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                    }
                                                                                                                    (lam
                                                                                                                      ds
                                                                                                                      [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                      (lam
                                                                                                                        ds
                                                                                                                        [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                        [
                                                                                                                          [
                                                                                                                            fAdditiveMonoidValue
                                                                                                                            ds
                                                                                                                          ]
                                                                                                                          ds
                                                                                                                        ]
                                                                                                                      )
                                                                                                                    )
                                                                                                                  ]
                                                                                                                )
                                                                                                              )
                                                                                                              (let
                                                                                                                (nonrec
                                                                                                                )
                                                                                                                (termbind
                                                                                                                  (strict
                                                                                                                  )
                                                                                                                  (vardecl
                                                                                                                    isZero
                                                                                                                    (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] Bool)
                                                                                                                  )
                                                                                                                  (lam
                                                                                                                    ds
                                                                                                                    [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                    (let
                                                                                                                      (rec
                                                                                                                      )
                                                                                                                      (termbind
                                                                                                                        (strict
                                                                                                                        )
                                                                                                                        (vardecl
                                                                                                                          go
                                                                                                                          (fun [List [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]] Bool)
                                                                                                                        )
                                                                                                                        (lam
                                                                                                                          xs
                                                                                                                          [List [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]]
                                                                                                                          [
                                                                                                                            [
                                                                                                                              [
                                                                                                                                {
                                                                                                                                  [
                                                                                                                                    {
                                                                                                                                      Nil_match
                                                                                                                                      [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                    }
                                                                                                                                    xs
                                                                                                                                  ]
                                                                                                                                  (fun Unit Bool)
                                                                                                                                }
                                                                                                                                (lam
                                                                                                                                  thunk
                                                                                                                                  Unit
                                                                                                                                  True
                                                                                                                                )
                                                                                                                              ]
                                                                                                                              (lam
                                                                                                                                ds
                                                                                                                                [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                (lam
                                                                                                                                  xs
                                                                                                                                  [List [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]]
                                                                                                                                  (lam
                                                                                                                                    thunk
                                                                                                                                    Unit
                                                                                                                                    [
                                                                                                                                      {
                                                                                                                                        [
                                                                                                                                          {
                                                                                                                                            {
                                                                                                                                              Tuple2_match
                                                                                                                                              (con bytestring)
                                                                                                                                            }
                                                                                                                                            [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]
                                                                                                                                          }
                                                                                                                                          ds
                                                                                                                                        ]
                                                                                                                                        Bool
                                                                                                                                      }
                                                                                                                                      (lam
                                                                                                                                        ds
                                                                                                                                        (con bytestring)
                                                                                                                                        (lam
                                                                                                                                          x
                                                                                                                                          [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]
                                                                                                                                          (let
                                                                                                                                            (rec
                                                                                                                                            )
                                                                                                                                            (termbind
                                                                                                                                              (strict
                                                                                                                                              )
                                                                                                                                              (vardecl
                                                                                                                                                go
                                                                                                                                                (fun [List [[Tuple2 (con bytestring)] (con integer)]] Bool)
                                                                                                                                              )
                                                                                                                                              (lam
                                                                                                                                                xs
                                                                                                                                                [List [[Tuple2 (con bytestring)] (con integer)]]
                                                                                                                                                [
                                                                                                                                                  [
                                                                                                                                                    [
                                                                                                                                                      {
                                                                                                                                                        [
                                                                                                                                                          {
                                                                                                                                                            Nil_match
                                                                                                                                                            [[Tuple2 (con bytestring)] (con integer)]
                                                                                                                                                          }
                                                                                                                                                          xs
                                                                                                                                                        ]
                                                                                                                                                        (fun Unit Bool)
                                                                                                                                                      }
                                                                                                                                                      (lam
                                                                                                                                                        thunk
                                                                                                                                                        Unit
                                                                                                                                                        [
                                                                                                                                                          go
                                                                                                                                                          xs
                                                                                                                                                        ]
                                                                                                                                                      )
                                                                                                                                                    ]
                                                                                                                                                    (lam
                                                                                                                                                      ds
                                                                                                                                                      [[Tuple2 (con bytestring)] (con integer)]
                                                                                                                                                      (lam
                                                                                                                                                        xs
                                                                                                                                                        [List [[Tuple2 (con bytestring)] (con integer)]]
                                                                                                                                                        (lam
                                                                                                                                                          thunk
                                                                                                                                                          Unit
                                                                                                                                                          [
                                                                                                                                                            {
                                                                                                                                                              [
                                                                                                                                                                {
                                                                                                                                                                  {
                                                                                                                                                                    Tuple2_match
                                                                                                                                                                    (con bytestring)
                                                                                                                                                                  }
                                                                                                                                                                  (con integer)
                                                                                                                                                                }
                                                                                                                                                                ds
                                                                                                                                                              ]
                                                                                                                                                              Bool
                                                                                                                                                            }
                                                                                                                                                            (lam
                                                                                                                                                              ds
                                                                                                                                                              (con bytestring)
                                                                                                                                                              (lam
                                                                                                                                                                x
                                                                                                                                                                (con integer)
                                                                                                                                                                [
                                                                                                                                                                  [
                                                                                                                                                                    [
                                                                                                                                                                      {
                                                                                                                                                                        [
                                                                                                                                                                          Bool_match
                                                                                                                                                                          [
                                                                                                                                                                            [
                                                                                                                                                                              equalsInteger
                                                                                                                                                                              (con
                                                                                                                                                                                0
                                                                                                                                                                              )
                                                                                                                                                                            ]
                                                                                                                                                                            x
                                                                                                                                                                          ]
                                                                                                                                                                        ]
                                                                                                                                                                        (fun Unit Bool)
                                                                                                                                                                      }
                                                                                                                                                                      (lam
                                                                                                                                                                        thunk
                                                                                                                                                                        Unit
                                                                                                                                                                        [
                                                                                                                                                                          go
                                                                                                                                                                          xs
                                                                                                                                                                        ]
                                                                                                                                                                      )
                                                                                                                                                                    ]
                                                                                                                                                                    (lam
                                                                                                                                                                      thunk
                                                                                                                                                                      Unit
                                                                                                                                                                      False
                                                                                                                                                                    )
                                                                                                                                                                  ]
                                                                                                                                                                  Unit
                                                                                                                                                                ]
                                                                                                                                                              )
                                                                                                                                                            )
                                                                                                                                                          ]
                                                                                                                                                        )
                                                                                                                                                      )
                                                                                                                                                    )
                                                                                                                                                  ]
                                                                                                                                                  Unit
                                                                                                                                                ]
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                            [
                                                                                                                                              go
                                                                                                                                              x
                                                                                                                                            ]
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    ]
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                            ]
                                                                                                                            Unit
                                                                                                                          ]
                                                                                                                        )
                                                                                                                      )
                                                                                                                      [
                                                                                                                        go
                                                                                                                        ds
                                                                                                                      ]
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                (let
                                                                                                                  (nonrec
                                                                                                                  )
                                                                                                                  (termbind
                                                                                                                    (strict
                                                                                                                    )
                                                                                                                    (vardecl
                                                                                                                      lessThanInteger
                                                                                                                      (fun (con integer) (fun (con integer) Bool))
                                                                                                                    )
                                                                                                                    (lam
                                                                                                                      arg
                                                                                                                      (con integer)
                                                                                                                      (lam
                                                                                                                        arg
                                                                                                                        (con integer)
                                                                                                                        [
                                                                                                                          (lam
                                                                                                                            b
                                                                                                                            (all a (type) (fun a (fun a a)))
                                                                                                                            [
                                                                                                                              [
                                                                                                                                {
                                                                                                                                  b
                                                                                                                                  Bool
                                                                                                                                }
                                                                                                                                True
                                                                                                                              ]
                                                                                                                              False
                                                                                                                            ]
                                                                                                                          )
                                                                                                                          [
                                                                                                                            [
                                                                                                                              (builtin
                                                                                                                                lessThanInteger
                                                                                                                              )
                                                                                                                              arg
                                                                                                                            ]
                                                                                                                            arg
                                                                                                                          ]
                                                                                                                        ]
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                  (let
                                                                                                                    (nonrec
                                                                                                                    )
                                                                                                                    (termbind
                                                                                                                      (strict
                                                                                                                      )
                                                                                                                      (vardecl
                                                                                                                        lt
                                                                                                                        (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] Bool))
                                                                                                                      )
                                                                                                                      (lam
                                                                                                                        l
                                                                                                                        [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                        (lam
                                                                                                                          r
                                                                                                                          [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                          [
                                                                                                                            [
                                                                                                                              [
                                                                                                                                {
                                                                                                                                  [
                                                                                                                                    Bool_match
                                                                                                                                    [
                                                                                                                                      isZero
                                                                                                                                      l
                                                                                                                                    ]
                                                                                                                                  ]
                                                                                                                                  (fun Unit Bool)
                                                                                                                                }
                                                                                                                                (lam
                                                                                                                                  thunk
                                                                                                                                  Unit
                                                                                                                                  [
                                                                                                                                    [
                                                                                                                                      [
                                                                                                                                        {
                                                                                                                                          [
                                                                                                                                            Bool_match
                                                                                                                                            [
                                                                                                                                              isZero
                                                                                                                                              r
                                                                                                                                            ]
                                                                                                                                          ]
                                                                                                                                          (fun Unit Bool)
                                                                                                                                        }
                                                                                                                                        (lam
                                                                                                                                          thunk
                                                                                                                                          Unit
                                                                                                                                          False
                                                                                                                                        )
                                                                                                                                      ]
                                                                                                                                      (lam
                                                                                                                                        thunk
                                                                                                                                        Unit
                                                                                                                                        [
                                                                                                                                          [
                                                                                                                                            [
                                                                                                                                              checkBinRel
                                                                                                                                              lessThanInteger
                                                                                                                                            ]
                                                                                                                                            l
                                                                                                                                          ]
                                                                                                                                          r
                                                                                                                                        ]
                                                                                                                                      )
                                                                                                                                    ]
                                                                                                                                    Unit
                                                                                                                                  ]
                                                                                                                                )
                                                                                                                              ]
                                                                                                                              (lam
                                                                                                                                thunk
                                                                                                                                Unit
                                                                                                                                [
                                                                                                                                  [
                                                                                                                                    [
                                                                                                                                      checkBinRel
                                                                                                                                      lessThanInteger
                                                                                                                                    ]
                                                                                                                                    l
                                                                                                                                  ]
                                                                                                                                  r
                                                                                                                                ]
                                                                                                                              )
                                                                                                                            ]
                                                                                                                            Unit
                                                                                                                          ]
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                    (let
                                                                                                                      (nonrec
                                                                                                                      )
                                                                                                                      (termbind
                                                                                                                        (strict
                                                                                                                        )
                                                                                                                        (vardecl
                                                                                                                          requiredMargin
                                                                                                                          (fun Future (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]))
                                                                                                                        )
                                                                                                                        (lam
                                                                                                                          ds
                                                                                                                          Future
                                                                                                                          (lam
                                                                                                                            spotPrice
                                                                                                                            [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                            [
                                                                                                                              {
                                                                                                                                [
                                                                                                                                  Future_match
                                                                                                                                  ds
                                                                                                                                ]
                                                                                                                                [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                              }
                                                                                                                              (lam
                                                                                                                                ds
                                                                                                                                (con integer)
                                                                                                                                (lam
                                                                                                                                  ds
                                                                                                                                  (con integer)
                                                                                                                                  (lam
                                                                                                                                    ds
                                                                                                                                    [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                    (lam
                                                                                                                                      ds
                                                                                                                                      [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                      (lam
                                                                                                                                        ds
                                                                                                                                        (con bytestring)
                                                                                                                                        (lam
                                                                                                                                          ds
                                                                                                                                          [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                          [
                                                                                                                                            [
                                                                                                                                              [
                                                                                                                                                unionWith
                                                                                                                                                addInteger
                                                                                                                                              ]
                                                                                                                                              ds
                                                                                                                                            ]
                                                                                                                                            [
                                                                                                                                              [
                                                                                                                                                fAdditiveGroupValue_cscale
                                                                                                                                                ds
                                                                                                                                              ]
                                                                                                                                              [
                                                                                                                                                [
                                                                                                                                                  fAdditiveGroupValue
                                                                                                                                                  spotPrice
                                                                                                                                                ]
                                                                                                                                                ds
                                                                                                                                              ]
                                                                                                                                            ]
                                                                                                                                          ]
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                            ]
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                      (let
                                                                                                                        (nonrec
                                                                                                                        )
                                                                                                                        (termbind
                                                                                                                          (strict
                                                                                                                          )
                                                                                                                          (vardecl
                                                                                                                            violatingRole
                                                                                                                            (fun Future (fun Margins (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] [Maybe Role])))
                                                                                                                          )
                                                                                                                          (lam
                                                                                                                            future
                                                                                                                            Future
                                                                                                                            (lam
                                                                                                                              margins
                                                                                                                              Margins
                                                                                                                              (lam
                                                                                                                                spotPrice
                                                                                                                                [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                (let
                                                                                                                                  (nonrec
                                                                                                                                  )
                                                                                                                                  (termbind
                                                                                                                                    (nonstrict
                                                                                                                                    )
                                                                                                                                    (vardecl
                                                                                                                                      minMargin
                                                                                                                                      [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                    )
                                                                                                                                    [
                                                                                                                                      [
                                                                                                                                        requiredMargin
                                                                                                                                        future
                                                                                                                                      ]
                                                                                                                                      spotPrice
                                                                                                                                    ]
                                                                                                                                  )
                                                                                                                                  [
                                                                                                                                    [
                                                                                                                                      [
                                                                                                                                        {
                                                                                                                                          [
                                                                                                                                            Bool_match
                                                                                                                                            [
                                                                                                                                              [
                                                                                                                                                lt
                                                                                                                                                [
                                                                                                                                                  {
                                                                                                                                                    [
                                                                                                                                                      Margins_match
                                                                                                                                                      margins
                                                                                                                                                    ]
                                                                                                                                                    [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                  }
                                                                                                                                                  (lam
                                                                                                                                                    ds
                                                                                                                                                    [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                    (lam
                                                                                                                                                      ds
                                                                                                                                                      [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                      ds
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                                ]
                                                                                                                                              ]
                                                                                                                                              minMargin
                                                                                                                                            ]
                                                                                                                                          ]
                                                                                                                                          (fun Unit [Maybe Role])
                                                                                                                                        }
                                                                                                                                        (lam
                                                                                                                                          thunk
                                                                                                                                          Unit
                                                                                                                                          [
                                                                                                                                            {
                                                                                                                                              Just
                                                                                                                                              Role
                                                                                                                                            }
                                                                                                                                            Short
                                                                                                                                          ]
                                                                                                                                        )
                                                                                                                                      ]
                                                                                                                                      (lam
                                                                                                                                        thunk
                                                                                                                                        Unit
                                                                                                                                        [
                                                                                                                                          [
                                                                                                                                            [
                                                                                                                                              {
                                                                                                                                                [
                                                                                                                                                  Bool_match
                                                                                                                                                  [
                                                                                                                                                    [
                                                                                                                                                      lt
                                                                                                                                                      [
                                                                                                                                                        {
                                                                                                                                                          [
                                                                                                                                                            Margins_match
                                                                                                                                                            margins
                                                                                                                                                          ]
                                                                                                                                                          [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                        }
                                                                                                                                                        (lam
                                                                                                                                                          ds
                                                                                                                                                          [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                          (lam
                                                                                                                                                            ds
                                                                                                                                                            [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                            ds
                                                                                                                                                          )
                                                                                                                                                        )
                                                                                                                                                      ]
                                                                                                                                                    ]
                                                                                                                                                    minMargin
                                                                                                                                                  ]
                                                                                                                                                ]
                                                                                                                                                (fun Unit [Maybe Role])
                                                                                                                                              }
                                                                                                                                              (lam
                                                                                                                                                thunk
                                                                                                                                                Unit
                                                                                                                                                [
                                                                                                                                                  {
                                                                                                                                                    Just
                                                                                                                                                    Role
                                                                                                                                                  }
                                                                                                                                                  Long
                                                                                                                                                ]
                                                                                                                                              )
                                                                                                                                            ]
                                                                                                                                            (lam
                                                                                                                                              thunk
                                                                                                                                              Unit
                                                                                                                                              {
                                                                                                                                                Nothing
                                                                                                                                                Role
                                                                                                                                              }
                                                                                                                                            )
                                                                                                                                          ]
                                                                                                                                          Unit
                                                                                                                                        ]
                                                                                                                                      )
                                                                                                                                    ]
                                                                                                                                    Unit
                                                                                                                                  ]
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                        (let
                                                                                                                          (nonrec
                                                                                                                          )
                                                                                                                          (termbind
                                                                                                                            (strict
                                                                                                                            )
                                                                                                                            (vardecl
                                                                                                                              transition
                                                                                                                              (fun Future (fun FutureAccounts (fun [OldState FutureState] (fun FutureAction [Maybe [NewState FutureState]]))))
                                                                                                                            )
                                                                                                                            (lam
                                                                                                                              future
                                                                                                                              Future
                                                                                                                              (lam
                                                                                                                                owners
                                                                                                                                FutureAccounts
                                                                                                                                (lam
                                                                                                                                  ds
                                                                                                                                  [OldState FutureState]
                                                                                                                                  (lam
                                                                                                                                    i
                                                                                                                                    FutureAction
                                                                                                                                    [
                                                                                                                                      {
                                                                                                                                        [
                                                                                                                                          {
                                                                                                                                            OldState_match
                                                                                                                                            FutureState
                                                                                                                                          }
                                                                                                                                          ds
                                                                                                                                        ]
                                                                                                                                        [Maybe [NewState FutureState]]
                                                                                                                                      }
                                                                                                                                      (lam
                                                                                                                                        ds
                                                                                                                                        FutureState
                                                                                                                                        (lam
                                                                                                                                          ds
                                                                                                                                          [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                          (let
                                                                                                                                            (nonrec
                                                                                                                                            )
                                                                                                                                            (termbind
                                                                                                                                              (nonstrict
                                                                                                                                              )
                                                                                                                                              (vardecl
                                                                                                                                                ftDeliveryDate
                                                                                                                                                (con integer)
                                                                                                                                              )
                                                                                                                                              [
                                                                                                                                                {
                                                                                                                                                  [
                                                                                                                                                    Future_match
                                                                                                                                                    future
                                                                                                                                                  ]
                                                                                                                                                  (con integer)
                                                                                                                                                }
                                                                                                                                                (lam
                                                                                                                                                  ds
                                                                                                                                                  (con integer)
                                                                                                                                                  (lam
                                                                                                                                                    ds
                                                                                                                                                    (con integer)
                                                                                                                                                    (lam
                                                                                                                                                      ds
                                                                                                                                                      [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                      (lam
                                                                                                                                                        ds
                                                                                                                                                        [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                        (lam
                                                                                                                                                          ds
                                                                                                                                                          (con bytestring)
                                                                                                                                                          (lam
                                                                                                                                                            ds
                                                                                                                                                            [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                            ds
                                                                                                                                                          )
                                                                                                                                                        )
                                                                                                                                                      )
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                              ]
                                                                                                                                            )
                                                                                                                                            [
                                                                                                                                              [
                                                                                                                                                [
                                                                                                                                                  {
                                                                                                                                                    [
                                                                                                                                                      FutureState_match
                                                                                                                                                      ds
                                                                                                                                                    ]
                                                                                                                                                    (fun Unit [Maybe [NewState FutureState]])
                                                                                                                                                  }
                                                                                                                                                  (lam
                                                                                                                                                    thunk
                                                                                                                                                    Unit
                                                                                                                                                    {
                                                                                                                                                      Nothing
                                                                                                                                                      [NewState FutureState]
                                                                                                                                                    }
                                                                                                                                                  )
                                                                                                                                                ]
                                                                                                                                                (lam
                                                                                                                                                  accounts
                                                                                                                                                  Margins
                                                                                                                                                  (lam
                                                                                                                                                    thunk
                                                                                                                                                    Unit
                                                                                                                                                    [
                                                                                                                                                      [
                                                                                                                                                        [
                                                                                                                                                          {
                                                                                                                                                            [
                                                                                                                                                              FutureAction_match
                                                                                                                                                              i
                                                                                                                                                            ]
                                                                                                                                                            [Maybe [NewState FutureState]]
                                                                                                                                                          }
                                                                                                                                                          (lam
                                                                                                                                                            role
                                                                                                                                                            Role
                                                                                                                                                            (lam
                                                                                                                                                              topUp
                                                                                                                                                              [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                              [
                                                                                                                                                                {
                                                                                                                                                                  Just
                                                                                                                                                                  [NewState FutureState]
                                                                                                                                                                }
                                                                                                                                                                [
                                                                                                                                                                  [
                                                                                                                                                                    [
                                                                                                                                                                      {
                                                                                                                                                                        NewState
                                                                                                                                                                        FutureState
                                                                                                                                                                      }
                                                                                                                                                                      [
                                                                                                                                                                        Running
                                                                                                                                                                        [
                                                                                                                                                                          [
                                                                                                                                                                            [
                                                                                                                                                                              adjustMargin
                                                                                                                                                                              role
                                                                                                                                                                            ]
                                                                                                                                                                            topUp
                                                                                                                                                                          ]
                                                                                                                                                                          accounts
                                                                                                                                                                        ]
                                                                                                                                                                      ]
                                                                                                                                                                    ]
                                                                                                                                                                    [
                                                                                                                                                                      [
                                                                                                                                                                        [
                                                                                                                                                                          unionWith
                                                                                                                                                                          addInteger
                                                                                                                                                                        ]
                                                                                                                                                                        topUp
                                                                                                                                                                      ]
                                                                                                                                                                      [
                                                                                                                                                                        totalMargin
                                                                                                                                                                        accounts
                                                                                                                                                                      ]
                                                                                                                                                                    ]
                                                                                                                                                                  ]
                                                                                                                                                                  {
                                                                                                                                                                    Nil
                                                                                                                                                                    TxConstraint
                                                                                                                                                                  }
                                                                                                                                                                ]
                                                                                                                                                              ]
                                                                                                                                                            )
                                                                                                                                                          )
                                                                                                                                                        ]
                                                                                                                                                        (lam
                                                                                                                                                          ov
                                                                                                                                                          [OracleValue [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]]
                                                                                                                                                          [
                                                                                                                                                            {
                                                                                                                                                              [
                                                                                                                                                                Future_match
                                                                                                                                                                future
                                                                                                                                                              ]
                                                                                                                                                              [Maybe [NewState FutureState]]
                                                                                                                                                            }
                                                                                                                                                            (lam
                                                                                                                                                              ds
                                                                                                                                                              (con integer)
                                                                                                                                                              (lam
                                                                                                                                                                ds
                                                                                                                                                                (con integer)
                                                                                                                                                                (lam
                                                                                                                                                                  ds
                                                                                                                                                                  [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                                  (lam
                                                                                                                                                                    ds
                                                                                                                                                                    [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                                    (lam
                                                                                                                                                                      ds
                                                                                                                                                                      (con bytestring)
                                                                                                                                                                      (lam
                                                                                                                                                                        ds
                                                                                                                                                                        [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                                        [
                                                                                                                                                                          {
                                                                                                                                                                            [
                                                                                                                                                                              {
                                                                                                                                                                                OracleValue_match
                                                                                                                                                                                [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                                              }
                                                                                                                                                                              ov
                                                                                                                                                                            ]
                                                                                                                                                                            [Maybe [NewState FutureState]]
                                                                                                                                                                          }
                                                                                                                                                                          (lam
                                                                                                                                                                            ds
                                                                                                                                                                            (con bytestring)
                                                                                                                                                                            (lam
                                                                                                                                                                              ds
                                                                                                                                                                              (con integer)
                                                                                                                                                                              (lam
                                                                                                                                                                                ds
                                                                                                                                                                                [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                                                [
                                                                                                                                                                                  [
                                                                                                                                                                                    [
                                                                                                                                                                                      {
                                                                                                                                                                                        [
                                                                                                                                                                                          Bool_match
                                                                                                                                                                                          [
                                                                                                                                                                                            [
                                                                                                                                                                                              equalsByteString
                                                                                                                                                                                              ds
                                                                                                                                                                                            ]
                                                                                                                                                                                            ds
                                                                                                                                                                                          ]
                                                                                                                                                                                        ]
                                                                                                                                                                                        (fun Unit [Maybe [NewState FutureState]])
                                                                                                                                                                                      }
                                                                                                                                                                                      (lam
                                                                                                                                                                                        thunk
                                                                                                                                                                                        Unit
                                                                                                                                                                                        [
                                                                                                                                                                                          [
                                                                                                                                                                                            [
                                                                                                                                                                                              {
                                                                                                                                                                                                [
                                                                                                                                                                                                  Bool_match
                                                                                                                                                                                                  [
                                                                                                                                                                                                    [
                                                                                                                                                                                                      equalsInteger
                                                                                                                                                                                                      ftDeliveryDate
                                                                                                                                                                                                    ]
                                                                                                                                                                                                    ds
                                                                                                                                                                                                  ]
                                                                                                                                                                                                ]
                                                                                                                                                                                                (fun Unit [Maybe [NewState FutureState]])
                                                                                                                                                                                              }
                                                                                                                                                                                              (lam
                                                                                                                                                                                                thunk
                                                                                                                                                                                                Unit
                                                                                                                                                                                                [
                                                                                                                                                                                                  {
                                                                                                                                                                                                    Just
                                                                                                                                                                                                    [NewState FutureState]
                                                                                                                                                                                                  }
                                                                                                                                                                                                  [
                                                                                                                                                                                                    [
                                                                                                                                                                                                      [
                                                                                                                                                                                                        {
                                                                                                                                                                                                          NewState
                                                                                                                                                                                                          FutureState
                                                                                                                                                                                                        }
                                                                                                                                                                                                        Finished
                                                                                                                                                                                                      ]
                                                                                                                                                                                                      {
                                                                                                                                                                                                        Nil
                                                                                                                                                                                                        [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                                                                      }
                                                                                                                                                                                                    ]
                                                                                                                                                                                                    [
                                                                                                                                                                                                      [
                                                                                                                                                                                                        {
                                                                                                                                                                                                          Cons
                                                                                                                                                                                                          TxConstraint
                                                                                                                                                                                                        }
                                                                                                                                                                                                        [
                                                                                                                                                                                                          MustValidateIn
                                                                                                                                                                                                          [
                                                                                                                                                                                                            {
                                                                                                                                                                                                              from
                                                                                                                                                                                                              (con integer)
                                                                                                                                                                                                            }
                                                                                                                                                                                                            ftDeliveryDate
                                                                                                                                                                                                          ]
                                                                                                                                                                                                        ]
                                                                                                                                                                                                      ]
                                                                                                                                                                                                      [
                                                                                                                                                                                                        [
                                                                                                                                                                                                          payoutsTx
                                                                                                                                                                                                          [
                                                                                                                                                                                                            {
                                                                                                                                                                                                              [
                                                                                                                                                                                                                Margins_match
                                                                                                                                                                                                                accounts
                                                                                                                                                                                                              ]
                                                                                                                                                                                                              Payouts
                                                                                                                                                                                                            }
                                                                                                                                                                                                            (lam
                                                                                                                                                                                                              ds
                                                                                                                                                                                                              [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                                                                              (lam
                                                                                                                                                                                                                ds
                                                                                                                                                                                                                [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                                                                                (let
                                                                                                                                                                                                                  (nonrec
                                                                                                                                                                                                                  )
                                                                                                                                                                                                                  (termbind
                                                                                                                                                                                                                    (nonstrict
                                                                                                                                                                                                                    )
                                                                                                                                                                                                                    (vardecl
                                                                                                                                                                                                                      delta
                                                                                                                                                                                                                      [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                                                                                    )
                                                                                                                                                                                                                    [
                                                                                                                                                                                                                      [
                                                                                                                                                                                                                        fAdditiveGroupValue_cscale
                                                                                                                                                                                                                        ds
                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                      [
                                                                                                                                                                                                                        [
                                                                                                                                                                                                                          fAdditiveGroupValue
                                                                                                                                                                                                                          ds
                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                        ds
                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                  )
                                                                                                                                                                                                                  [
                                                                                                                                                                                                                    [
                                                                                                                                                                                                                      Payouts
                                                                                                                                                                                                                      [
                                                                                                                                                                                                                        [
                                                                                                                                                                                                                          fAdditiveGroupValue
                                                                                                                                                                                                                          ds
                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                        delta
                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                    [
                                                                                                                                                                                                                      [
                                                                                                                                                                                                                        fAdditiveMonoidValue
                                                                                                                                                                                                                        ds
                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                      delta
                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                )
                                                                                                                                                                                                              )
                                                                                                                                                                                                            )
                                                                                                                                                                                                          ]
                                                                                                                                                                                                        ]
                                                                                                                                                                                                        owners
                                                                                                                                                                                                      ]
                                                                                                                                                                                                    ]
                                                                                                                                                                                                  ]
                                                                                                                                                                                                ]
                                                                                                                                                                                              )
                                                                                                                                                                                            ]
                                                                                                                                                                                            (lam
                                                                                                                                                                                              thunk
                                                                                                                                                                                              Unit
                                                                                                                                                                                              {
                                                                                                                                                                                                Nothing
                                                                                                                                                                                                [NewState FutureState]
                                                                                                                                                                                              }
                                                                                                                                                                                            )
                                                                                                                                                                                          ]
                                                                                                                                                                                          Unit
                                                                                                                                                                                        ]
                                                                                                                                                                                      )
                                                                                                                                                                                    ]
                                                                                                                                                                                    (lam
                                                                                                                                                                                      thunk
                                                                                                                                                                                      Unit
                                                                                                                                                                                      {
                                                                                                                                                                                        Nothing
                                                                                                                                                                                        [NewState FutureState]
                                                                                                                                                                                      }
                                                                                                                                                                                    )
                                                                                                                                                                                  ]
                                                                                                                                                                                  Unit
                                                                                                                                                                                ]
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                          )
                                                                                                                                                                        ]
                                                                                                                                                                      )
                                                                                                                                                                    )
                                                                                                                                                                  )
                                                                                                                                                                )
                                                                                                                                                              )
                                                                                                                                                            )
                                                                                                                                                          ]
                                                                                                                                                        )
                                                                                                                                                      ]
                                                                                                                                                      (lam
                                                                                                                                                        ov
                                                                                                                                                        [OracleValue [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]]
                                                                                                                                                        (let
                                                                                                                                                          (nonrec
                                                                                                                                                          )
                                                                                                                                                          (termbind
                                                                                                                                                            (strict
                                                                                                                                                            )
                                                                                                                                                            (vardecl
                                                                                                                                                              wild
                                                                                                                                                              Future
                                                                                                                                                            )
                                                                                                                                                            future
                                                                                                                                                          )
                                                                                                                                                          [
                                                                                                                                                            {
                                                                                                                                                              [
                                                                                                                                                                Future_match
                                                                                                                                                                future
                                                                                                                                                              ]
                                                                                                                                                              [Maybe [NewState FutureState]]
                                                                                                                                                            }
                                                                                                                                                            (lam
                                                                                                                                                              ds
                                                                                                                                                              (con integer)
                                                                                                                                                              (lam
                                                                                                                                                                ds
                                                                                                                                                                (con integer)
                                                                                                                                                                (lam
                                                                                                                                                                  ds
                                                                                                                                                                  [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                                  (lam
                                                                                                                                                                    ds
                                                                                                                                                                    [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                                    (lam
                                                                                                                                                                      ds
                                                                                                                                                                      (con bytestring)
                                                                                                                                                                      (lam
                                                                                                                                                                        ds
                                                                                                                                                                        [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                                        [
                                                                                                                                                                          {
                                                                                                                                                                            [
                                                                                                                                                                              {
                                                                                                                                                                                OracleValue_match
                                                                                                                                                                                [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                                              }
                                                                                                                                                                              ov
                                                                                                                                                                            ]
                                                                                                                                                                            [Maybe [NewState FutureState]]
                                                                                                                                                                          }
                                                                                                                                                                          (lam
                                                                                                                                                                            ds
                                                                                                                                                                            (con bytestring)
                                                                                                                                                                            (lam
                                                                                                                                                                              ds
                                                                                                                                                                              (con integer)
                                                                                                                                                                              (lam
                                                                                                                                                                                ds
                                                                                                                                                                                [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                                                [
                                                                                                                                                                                  [
                                                                                                                                                                                    [
                                                                                                                                                                                      {
                                                                                                                                                                                        [
                                                                                                                                                                                          Bool_match
                                                                                                                                                                                          [
                                                                                                                                                                                            [
                                                                                                                                                                                              equalsByteString
                                                                                                                                                                                              ds
                                                                                                                                                                                            ]
                                                                                                                                                                                            ds
                                                                                                                                                                                          ]
                                                                                                                                                                                        ]
                                                                                                                                                                                        (fun Unit [Maybe [NewState FutureState]])
                                                                                                                                                                                      }
                                                                                                                                                                                      (lam
                                                                                                                                                                                        thunk
                                                                                                                                                                                        Unit
                                                                                                                                                                                        [
                                                                                                                                                                                          [
                                                                                                                                                                                            [
                                                                                                                                                                                              {
                                                                                                                                                                                                [
                                                                                                                                                                                                  {
                                                                                                                                                                                                    Maybe_match
                                                                                                                                                                                                    Role
                                                                                                                                                                                                  }
                                                                                                                                                                                                  [
                                                                                                                                                                                                    [
                                                                                                                                                                                                      [
                                                                                                                                                                                                        violatingRole
                                                                                                                                                                                                        wild
                                                                                                                                                                                                      ]
                                                                                                                                                                                                      accounts
                                                                                                                                                                                                    ]
                                                                                                                                                                                                    ds
                                                                                                                                                                                                  ]
                                                                                                                                                                                                ]
                                                                                                                                                                                                (fun Unit [Maybe [NewState FutureState]])
                                                                                                                                                                                              }
                                                                                                                                                                                              (lam
                                                                                                                                                                                                vRole
                                                                                                                                                                                                Role
                                                                                                                                                                                                (lam
                                                                                                                                                                                                  thunk
                                                                                                                                                                                                  Unit
                                                                                                                                                                                                  [
                                                                                                                                                                                                    [
                                                                                                                                                                                                      [
                                                                                                                                                                                                        {
                                                                                                                                                                                                          [
                                                                                                                                                                                                            Bool_match
                                                                                                                                                                                                            [
                                                                                                                                                                                                              [
                                                                                                                                                                                                                greaterThanInteger
                                                                                                                                                                                                                ftDeliveryDate
                                                                                                                                                                                                              ]
                                                                                                                                                                                                              ds
                                                                                                                                                                                                            ]
                                                                                                                                                                                                          ]
                                                                                                                                                                                                          (fun Unit [Maybe [NewState FutureState]])
                                                                                                                                                                                                        }
                                                                                                                                                                                                        (lam
                                                                                                                                                                                                          thunk
                                                                                                                                                                                                          Unit
                                                                                                                                                                                                          [
                                                                                                                                                                                                            {
                                                                                                                                                                                                              Just
                                                                                                                                                                                                              [NewState FutureState]
                                                                                                                                                                                                            }
                                                                                                                                                                                                            [
                                                                                                                                                                                                              [
                                                                                                                                                                                                                [
                                                                                                                                                                                                                  {
                                                                                                                                                                                                                    NewState
                                                                                                                                                                                                                    FutureState
                                                                                                                                                                                                                  }
                                                                                                                                                                                                                  Finished
                                                                                                                                                                                                                ]
                                                                                                                                                                                                                {
                                                                                                                                                                                                                  Nil
                                                                                                                                                                                                                  [[Tuple2 (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                                                                                }
                                                                                                                                                                                                              ]
                                                                                                                                                                                                              [
                                                                                                                                                                                                                {
                                                                                                                                                                                                                  build
                                                                                                                                                                                                                  TxConstraint
                                                                                                                                                                                                                }
                                                                                                                                                                                                                (abs
                                                                                                                                                                                                                  a
                                                                                                                                                                                                                  (type)
                                                                                                                                                                                                                  (lam
                                                                                                                                                                                                                    c
                                                                                                                                                                                                                    (fun TxConstraint (fun a a))
                                                                                                                                                                                                                    (lam
                                                                                                                                                                                                                      n
                                                                                                                                                                                                                      a
                                                                                                                                                                                                                      [
                                                                                                                                                                                                                        [
                                                                                                                                                                                                                          c
                                                                                                                                                                                                                          [
                                                                                                                                                                                                                            [
                                                                                                                                                                                                                              [
                                                                                                                                                                                                                                {
                                                                                                                                                                                                                                  [
                                                                                                                                                                                                                                    Role_match
                                                                                                                                                                                                                                    vRole
                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                  (fun Unit TxConstraint)
                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                (lam
                                                                                                                                                                                                                                  thunk
                                                                                                                                                                                                                                  Unit
                                                                                                                                                                                                                                  [
                                                                                                                                                                                                                                    [
                                                                                                                                                                                                                                      [
                                                                                                                                                                                                                                        MustPayToOtherScript
                                                                                                                                                                                                                                        [
                                                                                                                                                                                                                                          {
                                                                                                                                                                                                                                            [
                                                                                                                                                                                                                                              FutureAccounts_match
                                                                                                                                                                                                                                              owners
                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                            (con bytestring)
                                                                                                                                                                                                                                          }
                                                                                                                                                                                                                                          (lam
                                                                                                                                                                                                                                            ds
                                                                                                                                                                                                                                            [[Tuple2 (con bytestring)] (con bytestring)]
                                                                                                                                                                                                                                            (lam
                                                                                                                                                                                                                                              ds
                                                                                                                                                                                                                                              (con bytestring)
                                                                                                                                                                                                                                              (lam
                                                                                                                                                                                                                                                ds
                                                                                                                                                                                                                                                [[Tuple2 (con bytestring)] (con bytestring)]
                                                                                                                                                                                                                                                (lam
                                                                                                                                                                                                                                                  ds
                                                                                                                                                                                                                                                  (con bytestring)
                                                                                                                                                                                                                                                  ds
                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                              )
                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                          )
                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                      unitData
                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                    [
                                                                                                                                                                                                                                      {
                                                                                                                                                                                                                                        [
                                                                                                                                                                                                                                          Margins_match
                                                                                                                                                                                                                                          accounts
                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                        [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                                                                                                      }
                                                                                                                                                                                                                                      (lam
                                                                                                                                                                                                                                        ds
                                                                                                                                                                                                                                        [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                                                                                                        (lam
                                                                                                                                                                                                                                          ds
                                                                                                                                                                                                                                          [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                                                                                                          [
                                                                                                                                                                                                                                            [
                                                                                                                                                                                                                                              fAdditiveMonoidValue
                                                                                                                                                                                                                                              ds
                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                            ds
                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                      )
                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                )
                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                              (lam
                                                                                                                                                                                                                                thunk
                                                                                                                                                                                                                                Unit
                                                                                                                                                                                                                                [
                                                                                                                                                                                                                                  [
                                                                                                                                                                                                                                    [
                                                                                                                                                                                                                                      MustPayToOtherScript
                                                                                                                                                                                                                                      [
                                                                                                                                                                                                                                        {
                                                                                                                                                                                                                                          [
                                                                                                                                                                                                                                            FutureAccounts_match
                                                                                                                                                                                                                                            owners
                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                          (con bytestring)
                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                        (lam
                                                                                                                                                                                                                                          ds
                                                                                                                                                                                                                                          [[Tuple2 (con bytestring)] (con bytestring)]
                                                                                                                                                                                                                                          (lam
                                                                                                                                                                                                                                            ds
                                                                                                                                                                                                                                            (con bytestring)
                                                                                                                                                                                                                                            (lam
                                                                                                                                                                                                                                              ds
                                                                                                                                                                                                                                              [[Tuple2 (con bytestring)] (con bytestring)]
                                                                                                                                                                                                                                              (lam
                                                                                                                                                                                                                                                ds
                                                                                                                                                                                                                                                (con bytestring)
                                                                                                                                                                                                                                                ds
                                                                                                                                                                                                                                              )
                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                          )
                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                    unitData
                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                  [
                                                                                                                                                                                                                                    {
                                                                                                                                                                                                                                      [
                                                                                                                                                                                                                                        Margins_match
                                                                                                                                                                                                                                        accounts
                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                      [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                    (lam
                                                                                                                                                                                                                                      ds
                                                                                                                                                                                                                                      [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                                                                                                      (lam
                                                                                                                                                                                                                                        ds
                                                                                                                                                                                                                                        [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                                                                                                                                                                                                        [
                                                                                                                                                                                                                                          [
                                                                                                                                                                                                                                            fAdditiveMonoidValue
                                                                                                                                                                                                                                            ds
                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                          ds
                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                      )
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                              )
                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                            Unit
                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                        n
                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                    )
                                                                                                                                                                                                                  )
                                                                                                                                                                                                                )
                                                                                                                                                                                                              ]
                                                                                                                                                                                                            ]
                                                                                                                                                                                                          ]
                                                                                                                                                                                                        )
                                                                                                                                                                                                      ]
                                                                                                                                                                                                      (lam
                                                                                                                                                                                                        thunk
                                                                                                                                                                                                        Unit
                                                                                                                                                                                                        {
                                                                                                                                                                                                          Nothing
                                                                                                                                                                                                          [NewState FutureState]
                                                                                                                                                                                                        }
                                                                                                                                                                                                      )
                                                                                                                                                                                                    ]
                                                                                                                                                                                                    Unit
                                                                                                                                                                                                  ]
                                                                                                                                                                                                )
                                                                                                                                                                                              )
                                                                                                                                                                                            ]
                                                                                                                                                                                            (lam
                                                                                                                                                                                              thunk
                                                                                                                                                                                              Unit
                                                                                                                                                                                              {
                                                                                                                                                                                                Nothing
                                                                                                                                                                                                [NewState FutureState]
                                                                                                                                                                                              }
                                                                                                                                                                                            )
                                                                                                                                                                                          ]
                                                                                                                                                                                          Unit
                                                                                                                                                                                        ]
                                                                                                                                                                                      )
                                                                                                                                                                                    ]
                                                                                                                                                                                    (lam
                                                                                                                                                                                      thunk
                                                                                                                                                                                      Unit
                                                                                                                                                                                      {
                                                                                                                                                                                        Nothing
                                                                                                                                                                                        [NewState FutureState]
                                                                                                                                                                                      }
                                                                                                                                                                                    )
                                                                                                                                                                                  ]
                                                                                                                                                                                  Unit
                                                                                                                                                                                ]
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                          )
                                                                                                                                                                        ]
                                                                                                                                                                      )
                                                                                                                                                                    )
                                                                                                                                                                  )
                                                                                                                                                                )
                                                                                                                                                              )
                                                                                                                                                            )
                                                                                                                                                          ]
                                                                                                                                                        )
                                                                                                                                                      )
                                                                                                                                                    ]
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                              ]
                                                                                                                                              Unit
                                                                                                                                            ]
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    ]
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                          (let
                                                                                                                            (nonrec
                                                                                                                            )
                                                                                                                            (termbind
                                                                                                                              (strict
                                                                                                                              )
                                                                                                                              (vardecl
                                                                                                                                futureStateMachine
                                                                                                                                (fun Future (fun FutureAccounts [[StateMachine FutureState] FutureAction]))
                                                                                                                              )
                                                                                                                              (lam
                                                                                                                                ft
                                                                                                                                Future
                                                                                                                                (lam
                                                                                                                                  fos
                                                                                                                                  FutureAccounts
                                                                                                                                  [
                                                                                                                                    [
                                                                                                                                      [
                                                                                                                                        {
                                                                                                                                          {
                                                                                                                                            StateMachine
                                                                                                                                            FutureState
                                                                                                                                          }
                                                                                                                                          FutureAction
                                                                                                                                        }
                                                                                                                                        [
                                                                                                                                          [
                                                                                                                                            transition
                                                                                                                                            ft
                                                                                                                                          ]
                                                                                                                                          fos
                                                                                                                                        ]
                                                                                                                                      ]
                                                                                                                                      (lam
                                                                                                                                        ds
                                                                                                                                        FutureState
                                                                                                                                        [
                                                                                                                                          [
                                                                                                                                            [
                                                                                                                                              {
                                                                                                                                                [
                                                                                                                                                  FutureState_match
                                                                                                                                                  ds
                                                                                                                                                ]
                                                                                                                                                (fun Unit Bool)
                                                                                                                                              }
                                                                                                                                              (lam
                                                                                                                                                thunk
                                                                                                                                                Unit
                                                                                                                                                True
                                                                                                                                              )
                                                                                                                                            ]
                                                                                                                                            (lam
                                                                                                                                              ipv
                                                                                                                                              Margins
                                                                                                                                              (lam
                                                                                                                                                thunk
                                                                                                                                                Unit
                                                                                                                                                False
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          ]
                                                                                                                                          Unit
                                                                                                                                        ]
                                                                                                                                      )
                                                                                                                                    ]
                                                                                                                                    {
                                                                                                                                      {
                                                                                                                                        mkStateMachine
                                                                                                                                        FutureState
                                                                                                                                      }
                                                                                                                                      FutureAction
                                                                                                                                    }
                                                                                                                                  ]
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                            futureStateMachine
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)